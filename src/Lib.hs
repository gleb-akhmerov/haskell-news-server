{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.String ( fromString )
import Database.PostgreSQL.Simple ( connectPostgreSQL, execute_, begin, rollback )

someFunc :: IO ()
someFunc = do
  migrationSql <- readFile "migrations/1.sql"
  conn <- connectPostgreSQL "host='localhost' port='5432' dbname='haskell-news-server' user='postgres'"
  begin conn
  print =<< execute_ conn (fromString migrationSql)
  rollback conn
