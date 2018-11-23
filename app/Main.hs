{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Database.MySQL.Base
import           Lib

mysqlConnInfo =
  defaultConnectInfo
    { ciHost = "127.0.0.1"
    , ciUser = "peakseek"
    , ciPassword = "peakseek"
    , ciDatabase = "peakseek"
    }

main :: IO ()
main = connect mysqlConnInfo >>= startApp port . DBMysql
  where
    port = 8080
