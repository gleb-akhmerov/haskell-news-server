{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Applicative (liftA2)
import           Data.Bits (xor)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (unpack)
import           Data.ByteString.Lazy (toStrict)
import           Data.ByteString.Builder (byteString, stringUtf8)
import           Data.Int (Int32)
import           Data.Maybe (catMaybes)
import qualified Data.Set as Set (fromList)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8')
import qualified Data.Text as T (unpack)
import           Text.Read (readMaybe)

import Data.Aeson
import Database.Beam.Postgres
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd, FileInfo(..))

import Queries.Author
import Queries.Category
import Queries.Commentary
import Queries.Draft
import Queries.Photo
import Queries.Post
import Queries.Tag
import Queries.User
import Queries.Util


matchRoute :: ByteString -> [Text] -> ByteString -> [Text] -> Bool
matchRoute method path rMethod rPath =
  let match (p, rp) =
        (p == "*") `xor` (p == rp)
  in method == rMethod
     && length path == length rPath
     && all match (zip path rPath)

responseBs :: Status -> ResponseHeaders -> ByteString -> Response
responseBs s h = responseBuilder s h . byteString

notFound :: Response
notFound = responseBuilder status404 [] "Not found"

badRequest :: Response
badRequest = responseBuilder status400 [] "Bad request"

badRequestReason :: String -> Response
badRequestReason err = responseBuilder status400 [] (stringUtf8 err)

readMaybeText :: Read a => Text -> Maybe a
readMaybeText = readMaybe . T.unpack

readMaybeBs :: Read a => ByteString -> Maybe a
readMaybeBs = readMaybe . BS.unpack

responseJson :: ToJSON a => a -> Response
responseJson = responseBuilder
  status200
  [(hContentType, "application/json")]
  . fromEncoding . toEncoding

decodeUtf8Maybe :: ByteString -> Maybe Text
decodeUtf8Maybe = rightToMaybe . decodeUtf8'

hdlGetPhoto :: (Pg (Maybe ByteString) -> IO (Maybe ByteString)) -> Text -> IO Response
hdlGetPhoto runPg pIdText =
  case readMaybeText pIdText of
    Just pId -> do
      mPhotoBytes <- runPg $ getPhoto pId
      pure $ case mPhotoBytes of
        Nothing ->
          notFound
        Just photoBytes ->
          responseBs status200 [] photoBytes
    _ ->
      pure badRequest

hdlPostPhoto :: (Pg Int32 -> IO Int32) -> Request -> IO Response
hdlPostPhoto runPg req = do
  (_, files) <- parseRequestBody lbsBackEnd req
  case files of
    [(_,fileInfo)] -> do
      newId <- runPg $ createPhoto (toStrict (fileContent fileInfo))
      pure $ responseJson (object ["id" .= newId])
    _ ->
      pure badRequest

hdlDeleteCommentary :: (Pg (Either String ()) -> IO (Either String ())) -> Text -> Text -> IO Response
hdlDeleteCommentary runPg postIdText comIdText =
  case (readMaybeText postIdText, readMaybeText comIdText) of
    (Just postId, Just comId) -> do
      eitherRes <- runPg $ deleteCommentary postId comId
      pure $ case eitherRes of
        Left err ->
          badRequestReason err
        Right () ->
          responseBuilder status200 [] ""
    _ ->
      pure badRequest

hdlPostCommentary :: (Pg (Either String Int32) -> IO (Either String Int32)) -> Text -> Text -> Request -> IO Response
hdlPostCommentary runPg uIdText pIdText req = do
  (_, files) <- parseRequestBody lbsBackEnd req
  case files of
    [(_,fileInfo)] ->
      case (readMaybeText uIdText, readMaybeText pIdText, decode (fileContent fileInfo)) of
        (Just uId, Just pId, Just entity) -> do
          eitherRes <- runPg $ createCommentary uId pId entity
          pure $ case eitherRes of
            Left err ->
              badRequestReason err
            Right newId ->
              responseJson (object ["id" .= newId])
        _ ->
          pure badRequest
    _ ->
      pure badRequest

hdlPublishDraft :: (Pg (Either String Int32) -> IO (Either String Int32)) -> Text -> IO Response
hdlPublishDraft runPg dIdText =
  case readMaybeText dIdText of
    Just dId -> do
      eitherRes <- runPg $ publishDraft dId
      pure $ case eitherRes of
        Left err ->
          badRequestReason err
        Right newId ->
          responseJson (object ["id" .= newId])
    _ ->
      pure badRequest

maybeParseOrderBy :: ByteString -> Maybe PostOrderBy
maybeParseOrderBy x = case x of
  "published_at"  -> Just PoPublishedAt
  "author_name"   -> Just PoAuthorName
  "category_name" -> Just PoCategoryName
  "photo_count"   -> Just PoPhotoCount
  _               -> Nothing

maybeParseOrder :: ByteString -> Maybe Order
maybeParseOrder x = case x of
  "asc"  -> Just Ascending
  "desc" -> Just Descending
  _      -> Nothing

maybeParseFilter :: SimpleQueryItem -> Maybe PostFilter
maybeParseFilter (k, v) = case k of
  "published_at"     -> PfPublishedAt   <$> readMaybeBs v
  "published_at__lt" -> PfPublishedAtLt <$> readMaybeBs v
  "published_at__gt" -> PfPublishedAtGt <$> readMaybeBs v
  "author_name"      -> PfAuthorName    <$> decodeUtf8Maybe v
  "category"         -> PfCategoryId    <$> readMaybeBs v
  "tag"              -> PfTagId         <$> readMaybeBs v
  "tags__in"         -> PfTagIdsIn      <$> readMaybeBs v
  "tags__all"        -> PfTagIdsAll     <$> readMaybeBs v
  "post_name"        -> PfPostNameSubstring    <$> decodeUtf8Maybe v
  "post_content"     -> PfPostContentSubstring <$> decodeUtf8Maybe v
  "search"           -> PfSearchSubstring      <$> decodeUtf8Maybe v
  _                  -> Nothing

hdlGetPostsFiltered :: (Pg [ReturnedPost] -> IO [ReturnedPost]) -> SimpleQuery -> IO Response
hdlGetPostsFiltered runPg query = do
  let mOrder   = lookup "order"    query >>= maybeParseOrder
      mOrderBy = lookup "order_by" query >>= maybeParseOrderBy
      mPostOrder = liftA2 PostOrder mOrder mOrderBy
  let filters = Set.fromList (catMaybes (fmap maybeParseFilter query))
  posts <- runPg $ getPosts filters mPostOrder
  pure $ responseJson posts

hdlPostDraft :: (Pg (Either String Int32) -> IO (Either String Int32)) -> Text -> Request -> IO Response
hdlPostDraft runPg authorIdText req = do
  (_, files) <- parseRequestBody lbsBackEnd req
  case files of
    [(_,fileInfo)] ->
      case (readMaybeText authorIdText, decode (fileContent fileInfo)) of
        (Just authorId, Just entity) -> do
          eitherRes <- runPg $ createDraft authorId entity
          pure $ case eitherRes of
            Left err ->
              badRequestReason err
            Right newId ->
              responseJson (object ["id" .= newId])
        _ ->
          pure badRequest
    _ ->
      pure badRequest

hdlGetEntity :: ToJSON a => (Pg (Maybe a) -> IO (Maybe a)) -> (Int32 -> Pg (Maybe a)) -> Text -> IO Response
hdlGetEntity runPg getEntity eIdText =
  case readMaybeText eIdText of
    Nothing ->
      pure badRequest
    Just eId -> do
      mEntity <- runPg $ getEntity eId
      pure $ case mEntity of
        Nothing ->
          notFound
        Just entity ->
          responseJson entity

hdlGetAllEntities :: ToJSON a => (Pg [a] -> IO [a]) -> Pg [a] -> IO Response
hdlGetAllEntities runPg getAllEntities = do
  entities <- runPg getAllEntities
  pure $ responseJson entities

hdlPostEntityEither :: FromJSON a => (Pg (Either String Int32) -> IO (Either String Int32)) -> (a -> Pg (Either String Int32)) -> Request -> IO Response
hdlPostEntityEither runPg postEntity req = do
  (_, files) <- parseRequestBody lbsBackEnd req
  case files of
    [(_,fileInfo)] ->
      case decode (fileContent fileInfo) of
        Nothing ->
          pure badRequest
        Just entity -> do
          eitherRes <- runPg $ postEntity entity
          pure $ case eitherRes of
            Left err ->
              badRequestReason err
            Right newId ->
              responseJson (object ["id" .= newId])
    _ ->
      pure badRequest

hdlPostEntity :: FromJSON a => (Pg Int32 -> IO Int32) -> (a -> Pg Int32) -> Request -> IO Response
hdlPostEntity runPg postEntity req = do
  (_, files) <- parseRequestBody lbsBackEnd req
  case files of
    [(_,fileInfo)] ->
      case decode (fileContent fileInfo) of
        Nothing ->
          pure badRequest
        Just entity -> do
          newId <- runPg $ postEntity entity
          pure $ responseJson (object ["id" .= newId])
    _ ->
      pure badRequest

hdlDeleteEntity :: (Pg (Either String ()) -> IO (Either String ())) -> (Int32 -> Pg (Either String ())) -> Text -> IO Response
hdlDeleteEntity runPg deleteEntity eIdText =
  case readMaybeText eIdText of
    Nothing ->
      pure badRequest
    Just eId -> do
      eitherRes <- runPg $ deleteEntity eId
      pure $ case eitherRes of
        Left err ->
          badRequestReason err
        Right () ->
          responseBuilder status200 [] ""

hdlPutEntity :: FromJSON a => (Pg (Either String ()) -> IO (Either String ())) -> (Int32 -> a -> Pg (Either String ())) -> Text -> Request -> IO Response
hdlPutEntity runPg updateEntity eIdText req = do
  (_, files) <- parseRequestBody lbsBackEnd req
  case files of
    [(_,fileInfo)] ->
      case (readMaybeText eIdText, decode (fileContent fileInfo)) of
        (Just eId, Just entityUpd) -> do
          eitherRes <- runPg $ updateEntity eId entityUpd
          pure $ case eitherRes of
            Left err ->
              badRequestReason err
            Right newId ->
              responseJson (object ["id" .= newId])
        _ ->
          pure badRequest
    _ ->
      pure badRequest

main :: IO ()
main = do
  conn <- connectPostgreSQL "host='localhost' port='5432' dbname='haskell-news-server' user='postgres'"
  let runPg = runBeamPostgres conn
  run 3000 $ \req send ->
    let method = requestMethod req
        path = pathInfo req
    in do response <-
            case (method, path) of
              ("GET",    ["users"])           -> hdlGetAllEntities   runPg getAllUsers
              ("GET",    ["users", id_])      -> hdlGetEntity        runPg getUser id_
              ("POST",   ["users"])           -> hdlPostEntityEither runPg createUser req
              ("PUT",    ["users", id_])      -> hdlPutEntity        runPg updateUser id_ req
              ("DELETE", ["users", id_])      -> hdlDeleteEntity     runPg deleteUser id_

              ("GET",    ["authors"])         -> hdlGetAllEntities   runPg getAllAuthors
              ("GET",    ["authors", id_])    -> hdlGetEntity        runPg getAuthor id_
              ("POST",   ["authors"])         -> hdlPostEntityEither runPg createAuthor req
              ("PUT",    ["authors", id_])    -> hdlPutEntity        runPg updateAuthor id_ req
              ("DELETE", ["authors", id_])    -> hdlDeleteEntity     runPg deleteAuthor id_
              
              ("GET",    ["tags"])            -> hdlGetAllEntities   runPg getAllTags
              ("GET",    ["tags", id_])       -> hdlGetEntity        runPg getTag id_
              ("POST",   ["tags"])            -> hdlPostEntity       runPg createTag req
              ("PUT",    ["tags", id_])       -> hdlPutEntity        runPg updateTag id_ req
              ("DELETE", ["tags", id_])       -> hdlDeleteEntity     runPg deleteTag id_

              ("GET",    ["categories"])      -> hdlGetAllEntities   runPg getAllCategories
              ("GET",    ["categories", id_]) -> hdlGetEntity        runPg getCategory id_
              ("POST",   ["categories"])      -> hdlPostEntityEither runPg createCategory req
              ("PUT",    ["categories", id_]) -> hdlPutEntity        runPg updateCategory id_ req
              ("DELETE", ["categories", id_]) -> hdlDeleteEntity     runPg deleteCategory id_

              ("GET",    ["drafts"])          -> hdlGetAllEntities   runPg getAllDrafts
              ("GET",    ["drafts", id_])     -> hdlGetEntity        runPg getDraft id_
              ("POST",   ["drafts"])          -> hdlPostDraft        runPg undefined req
              ("POST",   ["drafts", id_, "publish"]) -> hdlPublishDraft runPg id_
              ("PUT",    ["drafts", id_])     -> hdlPutEntity        runPg updateDraft id_ req
              ("DELETE", ["drafts", id_])     -> hdlDeleteEntity     runPg deleteDraft id_

              ("GET",    ["photos", id_])     -> hdlGetPhoto         runPg id_
              ("POST",   ["photos"])          -> hdlPostPhoto        runPg req

              ("GET",    ["posts", id_, "comments"])      -> hdlGetEntity        runPg getPostCommentaries id_
              ("POST",   ["posts", id_, "comments"])      -> hdlPostCommentary   runPg undefined id_ req
              ("DELETE", ["posts", pId, "comments", cId]) -> hdlDeleteCommentary runPg pId cId
              
              ("GET",    ["posts"])           -> hdlGetPostsFiltered runPg (parseSimpleQuery (rawQueryString req))
              ("GET",    ["posts", id_])      -> hdlGetEntity        runPg getPost id_

              _ -> pure notFound
          send response
