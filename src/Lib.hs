{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
  ( startApp
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

data DataXY = DataXY
  { dataXYId :: Int
  , xs       :: [Float]
  , ys       :: [Float]
  }

$(deriveJSON defaultOptions ''DataXY)

type DB = TVar (Int, IntMap.IntMap DataXY)

type DataXYApi
   = "data" :> Get '[ JSON] [DataXY] :<|>
     "data" :> Capture "id" Int :> Get '[ JSON] DataXY :<|>
     "data" :> ReqBody '[ JSON] DataXY :> Post '[ JSON] DataXY :<|>
     "data" :> Capture "id" Int :> ReqBody '[ JSON] DataXY :> Put '[ JSON] ()

startApp :: IO ()
startApp = do
  db <- atomically $ newTVar (0, IntMap.empty)
  putStrLn $ "start server:" ++ show port
  run port $ app db
  where
    port = 8080

app :: DB -> Application
app db = serve api (server db)

api :: Proxy DataXYApi
api = Proxy

server :: DB -> Server DataXYApi
server db = getDataXYAll :<|> getDataXY :<|> postDataXY :<|> putDataXY
  where
    getDataXYAll = liftIO $ IntMap.elems . snd <$> readTVarIO db
    getDataXY did =
      liftIO $
      IntMap.findWithDefault (DataXY 0 [] []) did . snd <$> readTVarIO db
    postDataXY dataXY =
      liftIO . atomically $ do
        (maxId, m) <- readTVar db
        let newId = inc maxId (IntMap.keys m)
        let newDataXY = dataXY {dataXYId = newId}
        writeTVar db (newId, IntMap.insert newId newDataXY m)
        pure newDataXY
      where
        inc i ks =
          if i `elem` ks
            then inc (i + 1) ks
            else i
    putDataXY did dataXY =
      liftIO . atomically . modifyTVar db $ \(maxId, m) ->
        (max maxId did, IntMap.insert did dataXY m)
