{-# LANGUAGE OverloadedStrings #-}

module Main where


import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteString)
import Data.Int (Int32)
import Data.Text (Text)

import Data.Aeson (encode, ToJSON)
import Database.Beam.Postgres
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404, hContentType, Status, ResponseHeaders)

import Queries.Author
import Queries.Photo


matchRoute :: ByteString -> [Text] -> ByteString -> [Text] -> Bool
matchRoute method path rMethod rPath =
  let match (p, rp) =
        (p == "*") `xor` (p == rp)
  in method == rMethod
     && length path == length rPath
     && all match (zip path rPath)

responseBS :: Status -> ResponseHeaders -> ByteString -> Response
responseBS s h = responseBuilder s h . byteString

notFound :: Response
notFound = responseBuilder status404 [] "Not found"

appGetPhoto :: (Pg (Maybe ByteString) -> IO (Maybe ByteString)) -> Int32 -> IO Response
appGetPhoto runPg pId = do
  mPhotoBytes <- runPg $ getPhoto pId
  pure $ case mPhotoBytes of
    Nothing ->
      notFound
    Just photoBytes ->
      responseBS status200 [] photoBytes

appGetEntity :: ToJSON a => (Int32 -> Pg (Maybe a)) -> Int32 -> Pg Response
appGetEntity getEntity eId = do
  mEntity <- getEntity eId
  pure $ case mEntity of
    Nothing ->
      notFound
    Just entity ->
      responseLBS
        status200
        [(hContentType, "application/json")]
        (encode entity)

main :: IO ()
main = do
  conn <- connectPostgreSQL "host='localhost' port='5432' dbname='haskell-news-server' user='postgres'"
  run 3000 $ \req send ->
    let method = requestMethod req
        path = pathInfo req
    in do response <- runBeamPostgres conn $
            if matchRoute "GET" ["author", "*"] method path
            then appGetEntity getAuthor 1
            else pure notFound
          send response
