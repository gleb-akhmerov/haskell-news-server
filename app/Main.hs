{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Applicative (liftA2)
import           Data.Bits (xor)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (unpack)
import           Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as LBS (ByteString)
import           Data.ByteString.Builder (byteString, stringUtf8)
import           Data.Int (Int32)
import           Data.Maybe (catMaybes, isJust)
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

responseJson :: ToJSON a => a -> Response
responseJson = responseBuilder
  status200
  [(hContentType, "application/json")]
  . fromEncoding . toEncoding

responseBs :: Status -> ResponseHeaders -> ByteString -> Response
responseBs s h = responseBuilder s h . byteString

notFound :: Response
notFound = responseBuilder status404 [] "Not found"

badRequest :: Response
badRequest = responseBuilder status400 [] "Bad request"

badRequestReason :: String -> Response
badRequestReason err = responseBuilder status400 [] (stringUtf8 err)

forbidden :: Response
forbidden = responseBuilder status403 [] "Forbidden"

readMaybeText :: Read a => Text -> Maybe a
readMaybeText = readMaybe . T.unpack

readMaybeBs :: Read a => ByteString -> Maybe a
readMaybeBs = readMaybe . BS.unpack

decodeUtf8Maybe :: ByteString -> Maybe Text
decodeUtf8Maybe = rightToMaybe . decodeUtf8'

simpleQueryString :: Request -> SimpleQuery
simpleQueryString = parseSimpleQuery . rawQueryString

parseBody :: Request -> IO (Maybe LBS.ByteString)
parseBody req = do
  (_, files) <- parseRequestBody lbsBackEnd req
  pure $ case files of
           [(_,fileInfo)] -> Just (fileContent fileInfo)
           _              -> Nothing

hdlGetPhoto :: Text -> Pg Response
hdlGetPhoto pIdText =
  case readMaybeText pIdText of
    Just pId -> do
      mPhotoBytes <- getPhoto pId
      pure $ case mPhotoBytes of
        Nothing ->
          notFound
        Just photoBytes ->
          responseBs status200 [] photoBytes
    _ ->
      pure badRequest

hdlPostPhoto :: Maybe LBS.ByteString -> Pg Response
hdlPostPhoto mBody = do
  case mBody of
    Nothing ->
      pure badRequest
    Just body -> do
      newId <- createPhoto (toStrict body)
      pure $ responseJson (object ["id" .= newId])

hdlDeleteCommentary :: Text -> Text -> Pg Response
hdlDeleteCommentary postIdText comIdText =
  case (readMaybeText postIdText, readMaybeText comIdText) of
    (Just postId, Just comId) -> do
      eitherRes <- deleteCommentary postId comId
      pure $ case eitherRes of
        Left err ->
          badRequestReason err
        Right () ->
          responseBuilder status200 [] ""
    _ ->
      pure badRequest

hdlPostCommentary :: Text -> Text -> Maybe LBS.ByteString -> Pg Response
hdlPostCommentary uIdText pIdText mBody = do
  case (readMaybeText uIdText, readMaybeText pIdText, mBody >>= decode) of
    (Just uId, Just pId, Just entity) -> do
      eitherRes <- createCommentary uId pId entity
      pure $ case eitherRes of
        Left err ->
          badRequestReason err
        Right newId ->
          responseJson (object ["id" .= newId])
    _ ->
      pure badRequest

hdlPublishDraft :: Text -> Pg Response
hdlPublishDraft dIdText =
  case readMaybeText dIdText of
    Just dId -> do
      eitherRes <- publishDraft dId
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

hdlGetPostsFiltered :: SimpleQuery -> Pg Response
hdlGetPostsFiltered query = do
  let mOrder   = lookup "order"    query >>= maybeParseOrder
      mOrderBy = lookup "order_by" query >>= maybeParseOrderBy
      mPostOrder = liftA2 PostOrder mOrder mOrderBy
  let filters = Set.fromList (catMaybes (fmap maybeParseFilter query))
  posts <- getPosts filters mPostOrder
  pure $ responseJson posts

hdlPostDraft :: Text -> Maybe LBS.ByteString -> Pg Response
hdlPostDraft authorIdText mBody = do
  case (readMaybeText authorIdText, mBody >>= decode) of
    (Just authorId, Just entity) -> do
      eitherRes <- createDraft authorId entity
      pure $ case eitherRes of
        Left err ->
          badRequestReason err
        Right newId ->
          responseJson (object ["id" .= newId])
    _ ->
      pure badRequest

hdlGetEntity :: ToJSON a => (Int32 -> Pg (Maybe a)) -> Text -> Pg Response
hdlGetEntity getEntity eIdText =
  case readMaybeText eIdText of
    Nothing ->
      pure badRequest
    Just eId -> do
      mEntity <- getEntity eId
      pure $ case mEntity of
        Nothing ->
          notFound
        Just entity ->
          responseJson entity

hdlGetAllEntities :: ToJSON a => Pg [a] -> Pg Response
hdlGetAllEntities getAllEntities = do
  entities <- getAllEntities
  pure $ responseJson entities

hdlPostEntityEither :: FromJSON a => (a -> Pg (Either String Int32)) -> Maybe LBS.ByteString -> Pg Response
hdlPostEntityEither postEntity mBody = do
  case mBody >>= decode of
    Nothing ->
      pure badRequest
    Just entity -> do
      eitherRes <- postEntity entity
      pure $ case eitherRes of
        Left err ->
          badRequestReason err
        Right newId ->
          responseJson (object ["id" .= newId])

hdlPostEntity :: FromJSON a => (a -> Pg Int32) -> Maybe LBS.ByteString -> Pg Response
hdlPostEntity postEntity mBody = do
  case mBody >>= decode of
    Nothing ->
      pure badRequest
    Just entity -> do
      newId <- postEntity entity
      pure $ responseJson (object ["id" .= newId])

hdlDeleteEntity :: (Int32 -> Pg (Either String ())) -> Text -> Pg Response
hdlDeleteEntity deleteEntity eIdText =
  case readMaybeText eIdText of
    Nothing ->
      pure badRequest
    Just eId -> do
      eitherRes <- deleteEntity eId
      pure $ case eitherRes of
        Left err ->
          badRequestReason err
        Right () ->
          responseBuilder status200 [] ""

hdlPutEntity :: FromJSON a => (Int32 -> a -> Pg (Either String ())) -> Text -> Maybe LBS.ByteString -> Pg Response
hdlPutEntity updateEntity eIdText mBody = do
  case (readMaybeText eIdText, mBody >>= decode) of
    (Just eId, Just entityUpd) -> do
      eitherRes <- updateEntity eId entityUpd
      pure $ case eitherRes of
        Left err ->
          badRequestReason err
        Right newId ->
          responseJson (object ["id" .= newId])
    _ ->
      pure badRequest

data Auth
  = AAdmin
  | AAuthor
  | AUser
  deriving Eq

withAuth :: Auth -> SimpleQuery -> Pg Response -> Pg Response
withAuth auth query resp = do
  let mUserId = readMaybeBs =<< lookup "user_id" query
  case mUserId of
    Nothing ->
      pure forbidden
    Just userId -> do
      mUser <- getUser userId
      mAuthor <- getAuthor userId
      case mUser of
        Nothing ->
          pure forbidden
        Just user -> do
          let userAuth = if | rUserIsAdmin user -> AAdmin
                            | isJust mAuthor    -> AAuthor
                            | otherwise         -> AUser
          if | userAuth == auth -> resp
             | auth == AAdmin   -> pure notFound
             | otherwise        -> pure forbidden

withDraftAuth :: Text -> SimpleQuery -> Pg Response -> Pg Response
withDraftAuth draftIdText query resp =
  case (readMaybeText draftIdText, readMaybeBs =<< lookup "user_id" query) of
    (Just draftId, Just authorId) -> do
      b <- isDraftByAuthor draftId authorId
      if b
      then resp
      else pure forbidden
    _ ->
      pure forbidden

withCommentAuth :: Text -> SimpleQuery -> Pg Response -> Pg Response
withCommentAuth commentIdText query resp =
  case (readMaybeText commentIdText, readMaybeBs =<< lookup "user_id" query) of
    (Just commentId, Just userId) -> do
      b <- isCommentByUser commentId userId
      if b
      then resp
      else pure forbidden
    _ ->
      pure forbidden

main :: IO ()
main = do
  conn <- connectPostgreSQL "host='localhost' port='5432' dbname='haskell-news-server' user='postgres'"
  let runPg = runBeamPostgres conn
  run 3000 $ \req send -> do
    let method = requestMethod req
        path = pathInfo req
        query = simpleQueryString req
    mBody <- parseBody req
    response <- runPg $
      case (method, path) of
        ("GET",    ["users"])           -> withAuth AUser  query $ hdlGetAllEntities getAllUsers
        ("GET",    ["users", id_])      -> withAuth AUser  query $ hdlGetEntity getUser id_
        ("POST",   ["users"])           -> withAuth AUser  query $ hdlPostEntityEither createUser mBody
        ("DELETE", ["users", id_])      -> withAuth AAdmin query $ hdlDeleteEntity deleteUser id_

        ("GET",    ["authors"])         -> withAuth AAdmin query $ hdlGetAllEntities getAllAuthors
        ("GET",    ["authors", id_])    -> withAuth AAdmin query $ hdlGetEntity getAuthor id_
        ("POST",   ["authors"])         -> withAuth AAdmin query $ hdlPostEntityEither createAuthor mBody
        ("PUT",    ["authors", id_])    -> withAuth AAdmin query $ hdlPutEntity updateAuthor id_ mBody
        ("DELETE", ["authors", id_])    -> withAuth AAdmin query $ hdlDeleteEntity deleteAuthor id_
        
        ("GET",    ["tags"])            -> withAuth AUser  query $ hdlGetAllEntities getAllTags
        ("GET",    ["tags", id_])       -> withAuth AUser  query $ hdlGetEntity getTag id_
        ("POST",   ["tags"])            -> withAuth AAdmin query $ hdlPostEntity createTag mBody
        ("PUT",    ["tags", id_])       -> withAuth AAdmin query $ hdlPutEntity updateTag id_ mBody
        ("DELETE", ["tags", id_])       -> withAuth AAdmin query $ hdlDeleteEntity deleteTag id_

        ("GET",    ["categories"])      -> withAuth AUser  query $ hdlGetAllEntities getAllCategories
        ("GET",    ["categories", id_]) -> withAuth AUser  query $ hdlGetEntity getCategory id_
        ("POST",   ["categories"])      -> withAuth AAdmin query $ hdlPostEntityEither createCategory mBody
        ("PUT",    ["categories", id_]) -> withAuth AAdmin query $ hdlPutEntity updateCategory id_ mBody
        ("DELETE", ["categories", id_]) -> withAuth AAdmin query $ hdlDeleteEntity deleteCategory id_

        ("GET",    ["drafts"])          -> withAuth AAuthor query $ hdlGetAllEntities getAllDrafts
        ("GET",    ["drafts", id_])     -> withDraftAuth id_ query $ hdlGetEntity getDraft id_
        ("POST",   ["drafts"])          -> withAuth AAuthor query $ hdlPostDraft undefined mBody
        ("POST",   ["drafts", id_, "publish"]) -> withDraftAuth id_ query $ hdlPublishDraft id_
        ("PUT",    ["drafts", id_])     -> withDraftAuth id_ query $ hdlPutEntity updateDraft id_ mBody
        ("DELETE", ["drafts", id_])     -> withDraftAuth id_ query $ hdlDeleteEntity deleteDraft id_

        ("GET",    ["photos", id_])     -> withAuth AUser query $ hdlGetPhoto id_
        ("POST",   ["photos"])          -> withAuth AUser query $ hdlPostPhoto mBody

        ("GET",    ["posts", id_, "comments"])      -> withAuth AUser query $ hdlGetEntity getPostCommentaries id_
        ("POST",   ["posts", id_, "comments"])      -> withAuth AUser query $ hdlPostCommentary undefined id_ mBody
        ("DELETE", ["posts", pId, "comments", cId]) -> withCommentAuth cId query $ hdlDeleteCommentary pId cId
        
        ("GET",    ["posts"])           -> withAuth AUser query $ hdlGetPostsFiltered query
        ("GET",    ["posts", id_])      -> withAuth AUser query $ hdlGetEntity getPost id_

        _ -> pure notFound
    send response
