{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Database.MySQL.Base
import           Lib
import           System.Environment

mysqlConnInfo =
  defaultConnectInfo
    { ciHost = "127.0.0.1"
    , ciUser = "peakseek"
    , ciPassword = "peakseek"
    , ciDatabase = "peakseek"
    }

main :: IO ()
main = lookupEnv "PEAKSEEK_DB_HOST" >>= startApp port . DBMysqlCI . setHost
  where
    port = 8080
    setHost Nothing     = mysqlConnInfo
    setHost (Just host) = mysqlConnInfo {ciHost = host}
