{-# LANGUAGE DataKinds         #-}
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
import qualified Data.IntMap              as IntMap
import           Data.Text
import           Data.Time.Clock          (UTCTime, getCurrentTime)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import           Control.Arrow
import           Control.Monad
import           Data.Array
import           Data.List
import           Matrix.LU

import           Prelude                  as P

import           Database.MySQL.Base
import qualified System.IO.Streams        as Streams

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

data DBTVar =
  DBTVar (TVar (Int, IntMap.IntMap DataXY))

data DBMysql =
  DBMysql MySQLConn

class Store a where
  findById :: Int -> a -> IO DataXY
  findAll :: a -> IO [DataXY]
  create :: DataXY -> a -> IO DataXY
  updateById :: Int -> DataXY -> a -> IO ()
  deleteById :: Int -> a -> IO ()

type DataXYApi
   = "data" :> Get '[ JSON] [DataXY] :<|> "data" :> Capture "id" Int :> Get '[ JSON] DataXY :<|> "data" :> ReqBody '[ JSON] DataXY :> Post '[ JSON] DataXY :<|> "data" :> Capture "id" Int :> ReqBody '[ JSON] DataXY :> Put '[ JSON] () :<|> "data" :> Capture "id" Int :> Delete '[ JSON] () :<|> "data" :> Capture "id" Int :> "const" :> Capture "dim" Int :> Get '[ JSON] Const :<|> "data" :> Capture "id" Int :> "constn" :> Capture "dim" Int :> Get '[ JSON] ConstN :<|> "data" :> Capture "id" Int :> "pred" :> Capture "x" Double :> Get '[ JSON] Pred

startApp :: Store s => Int -> s -> IO ()
startApp port store = do
  run port $ Lib.app store

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

convertFromMysqlValueToDataXY :: [MySQLValue] -> DataXY
convertFromMysqlValueToDataXY _ = newDataXY

instance Store DBMysql where
  findById did (DBMysql conn) = do
    (defs, is) <- query_ conn "select * from dataxy"
    xs <- Streams.toList is
    return $ convertFromMysqlValueToDataXY $ P.head xs

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
