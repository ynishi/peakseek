{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
  ( startApp
  , DBMysql(..)
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.IntMap                            as IntMap
import           Data.Text
import           Data.Time.Clock                        (UTCTime,
                                                         getCurrentTime)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import qualified Network.Wai.Handler.Warp               as Warp
import           Network.Wai.Middleware.Cors            (CorsResourcePolicy (..),
                                                         cors,
                                                         corsExposedHeaders,
                                                         corsIgnoreFailures,
                                                         corsMaxAge,
                                                         corsMethods,
                                                         corsOrigins,
                                                         corsRequestHeaders,
                                                         corsRequireOrigin,
                                                         corsVaryOrigin,
                                                         simpleCorsResourcePolicy,
                                                         simpleHeaders)
import           Network.Wai.Middleware.Servant.Options (provideOptions)

import           Control.Arrow
import           Control.Monad
import           Data.Array
import           Data.List
import           Matrix.LU

import           Prelude                                as P

import           Database.MySQL.Base
import qualified System.IO.Streams                      as Streams

data DataXY = DataXY
  { dataXYId :: Maybe Int
  , xs       :: [Double]
  , ys       :: [Double]
  } deriving (Show)

data DataXYN = DataXYN
  { dataXYNId :: Maybe Int
  , xys       :: [DataXY]
  } deriving (Show)

data XY = XY
  { x :: Double
  , y :: Double
  } deriving (Show)

data Const = Const
  { constDataXYId :: Maybe Int
  , const         :: [Double]
  } deriving (Show)

data ConstN = ConstN
  { constNDataXYId :: Maybe Int
  , constN         :: [ConstElem]
  } deriving (Show)

data ConstElem = ConstElem
  { name  :: String
  , value :: Double
  } deriving (Show)

newtype Pred = Pred
  { xy :: XY
  }

$(deriveJSON defaultOptions ''DataXY)

$(deriveJSON defaultOptions ''XY)

$(deriveJSON defaultOptions ''Const)

$(deriveJSON defaultOptions ''ConstN)

$(deriveJSON defaultOptions ''ConstElem)

$(deriveJSON defaultOptions ''Pred)

newtype DBTVar =
  DBTVar (TVar (Int, IntMap.IntMap DataXY))

data DBMysql
  = DBMysql MySQLConn
  | DBMysqlCI ConnectInfo

class Store a where
  findById :: Int -> a -> IO DataXY
  findAll :: a -> IO [DataXY]
  create :: DataXY -> a -> IO DataXY
  updateById :: Int -> DataXY -> a -> IO ()
  deleteById :: Int -> a -> IO ()

type DataXYApi
   = "data" :> Get '[ JSON] [DataXY] :<|> "data" :> Capture "id" Int :> Get '[ JSON] DataXY :<|> "data" :> ReqBody '[ JSON] DataXY :> Post '[ JSON] DataXY :<|> "data" :> Capture "id" Int :> ReqBody '[ JSON] DataXY :> Put '[ JSON] () :<|> "data" :> Capture "id" Int :> Delete '[ JSON] () :<|> "data" :> Capture "id" Int :> "const" :> Capture "dim" Int :> Get '[ JSON] Const :<|> "data" :> Capture "id" Int :> "constn" :> Capture "dim" Int :> Get '[ JSON] ConstN :<|> "data" :> Capture "id" Int :> "pred" :> Capture "x" Double :> Get '[ JSON] Pred

startApp :: Store s => Int -> s -> IO ()
startApp port store =
  run port $
  cors (P.const $ Just allowAllMethodsPolicy) $
  provideOptions api $ Lib.app store
  where
    policy = simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}
    allowAllMethodsPolicy =
      CorsResourcePolicy
        { corsOrigins = Nothing
        , corsMethods = ["GET", "POST", "PUT", "DELETE", "HEAD", "OPTIONS"]
        , corsRequestHeaders = "access-control-allow-origin" : simpleHeaders
        , corsExposedHeaders = Nothing
        , corsMaxAge = Nothing
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
        }

app :: Store s => s -> Application
app db = serve api (server db)

api :: Proxy DataXYApi
api = Proxy

server :: Store s => s -> Server DataXYApi
server db =
  getDataXYAll :<|> getDataXY :<|> postDataXY :<|> putDataXY :<|> deleteDataXY :<|>
  getConst :<|>
  getConstN :<|>
  getPred
  where
    getDataXYAll = liftIO $ findAll db
    getDataXY did = liftIO $ findById did db
    postDataXY dataXY = liftIO $ create dataXY db
    putDataXY did dataXY = liftIO $ updateById did dataXY db
    deleteDataXY did = liftIO $ deleteById did db
    getConst did dim = liftIO $ calcConst dim <$> findById did db
    getConstN did dim = liftIO $ calcConstN dim <$> findById did db
    getPred did x = liftIO $ calcPred x <$> findById did db

fromMysqlValueToDataXY :: [[MySQLValue]] -> DataXY
fromMysqlValueToDataXY =
  rev .
  P.foldl
    (\z d@(MySQLInt32 i:MySQLDouble x:MySQLDouble y:_) ->
       z {dataXYId = Just $ fromIntegral i, xs = x : xs z, ys = y : ys z})
    newDataXY
  where
    rev d = d {xs = P.reverse (xs d), ys = P.reverse (ys d)}

fromMysqlValueToDataXYList :: [[MySQLValue]] -> [DataXY]
fromMysqlValueToDataXYList =
  P.map (rev . setId) .
  IntMap.toList .
  P.foldl
    (\z d@(MySQLInt32 i:MySQLDouble x:MySQLDouble y:_) ->
       IntMap.insertWith
         (\a1 a2 -> a1 {xs = xs a1 ++ xs a2, ys = ys a1 ++ ys a2})
         (fromIntegral i)
         (newDataXY {dataXYId = Nothing, xs = [x], ys = [y]})
         z)
    im
  where
    im :: IntMap.IntMap DataXY
    im = IntMap.empty
    rev d = d {xs = P.reverse (xs d), ys = P.reverse (ys d)}
    setId (i, d) = d {dataXYId = Just i}

connFromMysqlCI :: ConnectInfo -> IO DBMysql
connFromMysqlCI ci = do
  conn <- connect ci
  return $ DBMysql conn

instance Store DBMysql where
  findById did (DBMysqlCI ci) = connFromMysqlCI ci >>= findById did
  findById did (DBMysql conn) = do
    stmt <- prepareStmt conn "select * from dataxy where id = ?"
    (defs, is) <- queryStmt conn stmt [MySQLInt32 (fromIntegral did)]
    xs <- Streams.toList is
    print "found:"
    print xs
    return $ fromMysqlValueToDataXY xs
  findAll (DBMysqlCI ci) = connFromMysqlCI ci >>= findAll
  findAll (DBMysql conn) = do
    (defs, is) <- query_ conn "select * from dataxy"
    xs <- Streams.toList is
    print xs
    return $ fromMysqlValueToDataXYList xs
  create dataXY (DBMysqlCI ci) = connFromMysqlCI ci >>= create dataXY
  create dataXY (DBMysql conn) = do
    (_, maxId) <- query_ conn "select max(id) from dataxy"
    maxIdL <- Streams.toList maxId
    print maxIdL
    let maxi = inc . P.head . P.head $ maxIdL
    let newDataXY = setId maxi dataXY
    stmt <- prepareStmt conn "INSERT INTO dataxy values(?,?,?)"
    let xys = P.zip (xs dataXY) (ys dataXY)
    forM_ xys $ \xy ->
      executeStmt
        conn
        stmt
        [MySQLInt32 maxi, MySQLDouble . fst $ xy, MySQLDouble . snd $ xy]
    return newDataXY
    where
      inc MySQLNull      = 0
      inc (MySQLInt32 i) = i + 1
      setId did dataXY = dataXY {dataXYId = Just $ fromIntegral did}
  updateById did dataXY (DBMysqlCI ci) =
    connFromMysqlCI ci >>= updateById did dataXY
  updateById did dataXY (DBMysql conn) = do
    delStmt <- prepareStmt conn "DELETE from dataxy where id = ? and x = ?"
    insStmt <- prepareStmt conn "INSERT INTO dataxy values(?,?,?)"
    forM_ xys $ \xy -> do
      executeStmt
        conn
        delStmt
        [MySQLInt32 $ fromIntegral did, MySQLDouble . fst $ xy]
      executeStmt
        conn
        insStmt
        [ MySQLInt32 $ fromIntegral did
        , MySQLDouble . fst $ xy
        , MySQLDouble . snd $ xy
        ]
    where
      xys :: [(Double, Double)]
      xys = P.zip (xs dataXY) (ys dataXY)
  deleteById did (DBMysqlCI ci) = connFromMysqlCI ci >>= deleteById did
  deleteById did (DBMysql conn) = do
    stmt <- prepareStmt conn "DELETE from dataxy where id = ?"
    executeStmt conn stmt [MySQLInt32 $ fromIntegral did]
    return ()

instance Store DBTVar where
  findById did (DBTVar db) =
    IntMap.findWithDefault newDataXY did . snd <$> readTVarIO db
  findAll (DBTVar db) = IntMap.elems . snd <$> readTVarIO db
  create dataXY (DBTVar db) =
    atomically $ do
      (maxId, m) <- readTVar db
      let newId = inc maxId (IntMap.keys m)
      let newDataXY = dataXY {dataXYId = Just newId}
      writeTVar db (newId, IntMap.insert newId newDataXY m)
      pure newDataXY
    where
      inc i ks =
        if i `elem` ks
          then inc (i + 1) ks
          else i
  updateById did dataXY (DBTVar db) =
    atomically . modifyTVar db $ \(maxId, m) ->
      (max maxId did, IntMap.insert did dataXY m)
  deleteById did (DBTVar db) =
    atomically . modifyTVar db $ \(maxId, m) -> (maxId, IntMap.delete did m)

newDataXY :: DataXY
newDataXY = DataXY (Just 0) [] []

calcPred :: Double -> DataXY -> Pred
calcPred x dataXY = Pred (XY x (P.head cs + (cs !! 1) * x + (cs !! 2) * x * x))
  where
    cs = polyDataXY 3 dataXY

calcConst :: Int -> DataXY -> Const
calcConst dim dataXY = Const (dataXYId dataXY) . polyDataXY dim $ dataXY

calcConstN :: Int -> DataXY -> ConstN
calcConstN dim dataXY =
  ConstN (dataXYId dataXY) . P.zipWith (curry convert) ['a' ..] . polyDataXY dim $
  dataXY
  where
    convert (a, b) = ConstElem [a] b

polyDataXY :: Int -> DataXY -> [Double]
polyDataXY dim dataXY = polyfit dim (xs dataXY) (ys dataXY)

polyfit :: Int -> [Double] -> [Double] -> [Double]
polyfit d rx ry = elems $ solve mat vec
  where
    mat = listArray ((1, 1), (d, d)) $ liftM2 P.concatMap ppoly id $ P.take d rx
    vec = listArray (1, d) $ P.take d ry
    ppoly p x = Data.List.map (x **) p
