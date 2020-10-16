module Main where


import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS (unpack)
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString.Builder as BSB (byteString)
import           Data.Int (Int32)
import           Data.Maybe (catMaybes, isJust, fromMaybe)
import qualified Data.Set as Set (fromList)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text.Encoding as TSE (encodeUtf8, decodeUtf8')
import qualified Data.Text as TS (unpack)
import           Text.Read (readMaybe)

import           Data.Aeson
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple (execute_, begin)
import           Network.HTTP.Types
import           Network.Wai hiding (Response)
import qualified Network.Wai as W (Response)
import           Network.Wai.Handler.Warp (run)

import           NewsServer.Database.Author
import           NewsServer.Database.Category
import           NewsServer.Database.Commentary
import           NewsServer.Database.Draft
import           NewsServer.Database.Photo
import           NewsServer.Database.Post
import           NewsServer.Database.Tag
import           NewsServer.Database.User
import           NewsServer.Database.Util


data Response = Response Status ResponseHeaders BS.ByteString
  deriving Show

renderResponse :: Response -> W.Response
renderResponse (Response status headers bs) =
  responseBuilder status headers (BSB.byteString bs)

responseJsonOk :: ToJSON a => a -> Response
responseJsonOk = Response
  status200
  [(hContentType, "application/json")]
  . BL.toStrict . encode

responseJsonCreated :: ToJSON a => a -> Response
responseJsonCreated = Response
  status201
  [(hContentType, "application/json")]
  . BL.toStrict . encode

responseBs :: Status -> ResponseHeaders -> BS.ByteString -> Response
responseBs s h bs = Response s h bs

notFound :: Response
notFound = Response status404 [] "Not found"

badRequest :: Response
badRequest = Response status400 [] "Bad request"

badRequestReason :: Text -> Response
badRequestReason err = Response status400 [] (TSE.encodeUtf8 err)

forbidden :: Response
forbidden = Response status403 [] "Forbidden"

readMaybeText :: Read a => Text -> Maybe a
readMaybeText = readMaybe . TS.unpack

readMaybeBs :: Read a => BS.ByteString -> Maybe a
readMaybeBs = readMaybe . BS.unpack

decodeUtf8Maybe :: BS.ByteString -> Maybe Text
decodeUtf8Maybe = rightToMaybe . TSE.decodeUtf8'

simpleQueryString :: Request -> SimpleQuery
simpleQueryString = parseSimpleQuery . rawQueryString

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

hdlPostPhoto :: BL.ByteString -> Pg Response
hdlPostPhoto body = do
  newId <- createPhoto (BL.toStrict body)
  pure $ responseJsonCreated (object ["id" .= newId])

hdlDeleteCommentary :: Text -> Text -> Pg Response
hdlDeleteCommentary postIdText comIdText =
  case (readMaybeText postIdText, readMaybeText comIdText) of
    (Just postId, Just comId) -> do
      eitherRes <- deleteCommentary postId comId
      pure $ case eitherRes of
        Left err ->
          badRequestReason err
        Right () ->
          Response status200 [] ""
    _ ->
      pure badRequest

hdlPostCommentary :: Maybe BS.ByteString -> Text -> BL.ByteString -> Pg Response
hdlPostCommentary mUIdText pIdText body = do
  case (mUIdText >>= readMaybeBs, readMaybeText pIdText, decode body) of
    (Just uId, Just pId, Just entity) -> do
      eitherRes <- createCommentary uId pId entity
      pure $ case eitherRes of
        Left err ->
          badRequestReason err
        Right newId ->
          responseJsonCreated (object ["id" .= newId])
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
          responseJsonCreated (object ["id" .= newId])
    _ ->
      pure badRequest

maybeParseOrderBy :: BS.ByteString -> Maybe PostOrderBy
maybeParseOrderBy x = case x of
  "published_at"  -> Just PoPublishedAt
  "author_name"   -> Just PoAuthorName
  "category_name" -> Just PoCategoryName
  "photo_count"   -> Just PoPhotoCount
  _               -> Nothing

maybeParseOrder :: BS.ByteString -> Maybe Order
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
      mPostOrder = PostOrder <$> mOrder <*> mOrderBy
      filters = Set.fromList (catMaybes (fmap maybeParseFilter query))
      pageNum = fromMaybe 1 (readMaybeBs =<< lookup "page" query)
  posts <- getPosts filters mPostOrder pageNum
  pure $ responseJsonOk posts

hdlPostDraft :: Maybe BS.ByteString -> BL.ByteString -> Pg Response
hdlPostDraft mAuthorIdText body = do
  case (mAuthorIdText >>= readMaybeBs, decode body) of
    (Just authorId, Just entity) -> do
      eitherRes <- createDraft authorId entity
      pure $ case eitherRes of
        Left err ->
          badRequestReason err
        Right newId ->
          responseJsonCreated (object ["id" .= newId])
    _ ->
      pure badRequest

hdlGetAllDrafts :: SimpleQuery -> Pg Response
hdlGetAllDrafts query = do
  let pageNum = fromMaybe 1 (readMaybeBs =<< lookup "page" query)
  case readMaybeBs =<< lookup "user" query of
    Nothing ->
      pure badRequest
    Just authorId -> do
      entities <- getAllDrafts authorId pageNum
      pure $ responseJsonOk entities

hdlGetPostCommentaries :: SimpleQuery -> Text -> Pg Response
hdlGetPostCommentaries query postIdText = do
  let pageNum = fromMaybe 1 (readMaybeBs =<< lookup "page" query)
  case readMaybeText postIdText of
    Nothing ->
      pure badRequest
    Just postId -> do
      entities <- getPostCommentaries postId pageNum
      pure $ responseJsonOk entities

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
          responseJsonOk entity

hdlGetAllEntities :: ToJSON a => SimpleQuery -> (Integer -> Pg [a]) -> Pg Response
hdlGetAllEntities query getAllEntities = do
  let pageNum = fromMaybe 1 (readMaybeBs =<< lookup "page" query)
  entities <- getAllEntities pageNum
  pure $ responseJsonOk entities

hdlPostEntityEither :: FromJSON a => (a -> Pg (Either Text Int32)) -> BL.ByteString -> Pg Response
hdlPostEntityEither postEntity body = do
  case decode body of
    Nothing ->
      pure badRequest
    Just entity -> do
      eitherRes <- postEntity entity
      pure $ case eitherRes of
        Left err ->
          badRequestReason err
        Right newId ->
          responseJsonCreated (object ["id" .= newId])

hdlPostEntity :: FromJSON a => (a -> Pg Int32) -> BL.ByteString -> Pg Response
hdlPostEntity postEntity body = do
  case decode body of
    Nothing ->
      pure badRequest
    Just entity -> do
      newId <- postEntity entity
      pure $ responseJsonCreated (object ["id" .= newId])

hdlDeleteEntity :: (Int32 -> Pg (Either Text ())) -> Text -> Pg Response
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
          Response status200 [] ""

hdlPutEntity :: FromJSON a => (Int32 -> a -> Pg (Either Text ())) -> Text -> BL.ByteString -> Pg Response
hdlPutEntity updateEntity eIdText body = do
  case (readMaybeText eIdText, decode body) of
    (Just eId, Just entityUpd) -> do
      eitherRes <- updateEntity eId entityUpd
      pure $ case eitherRes of
        Left err ->
          badRequestReason err
        Right () ->
          Response status200 [] ""
    _ ->
      pure badRequest

data Auth
  = AUser
  | AAuthor
  | AAdmin
  deriving (Eq, Ord)

withAuth :: Auth -> SimpleQuery -> Pg Response -> Pg Response
withAuth auth query resp = do
  let mUserId = readMaybeBs =<< lookup "user" query
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
          if | userAuth >= auth -> resp
             | auth == AAdmin   -> pure notFound
             | otherwise        -> pure forbidden

withDraftAuth :: Text -> SimpleQuery -> Pg Response -> Pg Response
withDraftAuth draftIdText query resp =
  case (readMaybeText draftIdText, readMaybeBs =<< lookup "user" query) of
    (Just draftId, Just authorId) -> do
      b <- isDraftByAuthor draftId authorId
      if b
      then resp
      else pure forbidden
    _ ->
      pure forbidden

withCommentAuth :: Text -> SimpleQuery -> Pg Response -> Pg Response
withCommentAuth commentIdText query resp =
  case (readMaybeText commentIdText, readMaybeBs =<< lookup "user" query) of
    (Just commentId, Just userId) -> do
      b <- isCommentByUser commentId userId
      if b
      then resp
      else pure forbidden
    _ ->
      pure forbidden

data LogLevel
  = LevelInfo
  | LevelError

logLn :: LogLevel -> String -> IO ()
logLn level mes = do
  let l = case level of
            LevelInfo  -> "INFO "
            LevelError -> "ERROR"
  putStrLn $ l <> " " <> mes

main :: IO ()
main = do
  conn <- connectPostgreSQL "host='localhost' port='5432' dbname='haskell-news-server' user='postgres'"
  begin conn
  migration1 <- fromString <$> readFile "migrations/1.sql"
  migration2 <- fromString <$> readFile "migrations/2.sql"
  print =<< execute_ conn migration1
  print =<< execute_ conn migration2
  runBeamPostgres conn $ do
    photoId <- createPhoto ""
    Right userId <- createAdminUser CreateUser
      { cUserFirstName = "John"
      , cUserLastName = "Doe"
      , cUserAvatarId = photoId
      }
    Right authorId <- createAuthor CreateAuthor
      { cAuthorUserId = userId
      , cAuthorShortDescription = ""
      }
    Right categoryId <- createCategory CreateCategory
      { cCategoryParentId = Nothing
      , cCategoryName = "Haskell"
      }
    Right draftId <- createDraft authorId CreateDraft
      { cDraftShortName = ""
      , cDraftCategoryId = categoryId
      , cDraftTextContent = ""
      , cDraftMainPhotoId = photoId
      , cDraftAdditionalPhotoIds = []
      , cDraftTagIds = []
      }
    _ <- publishDraft draftId
    liftIO $ print draftId
    pure ()
  run 3000 $ \req send -> do
    let method = requestMethod req
        path = pathInfo req
        query = simpleQueryString req
    body <- strictRequestBody req
    logLn LevelInfo $ BS.unpack method <> " " <> show path <> " " <> show query <> " " <> show body
    response @ (Response status _ _) <- runBeamPostgres conn $
      case (method, path) of
        ("GET",    ["users"])           -> withAuth AUser  query $ hdlGetAllEntities query getAllUsers
        ("GET",    ["users", id_])      -> withAuth AUser  query $ hdlGetEntity getUser id_
        ("POST",   ["users"])           -> withAuth AUser  query $ hdlPostEntityEither createUser body
        ("DELETE", ["users", id_])      -> withAuth AAdmin query $ hdlDeleteEntity deleteUser id_

        ("GET",    ["authors"])         -> withAuth AAdmin query $ hdlGetAllEntities query getAllAuthors
        ("GET",    ["authors", id_])    -> withAuth AAdmin query $ hdlGetEntity getAuthor id_
        ("POST",   ["authors"])         -> withAuth AAdmin query $ hdlPostEntityEither createAuthor body
        ("PUT",    ["authors", id_])    -> withAuth AAdmin query $ hdlPutEntity updateAuthor id_ body
        ("DELETE", ["authors", id_])    -> withAuth AAdmin query $ hdlDeleteEntity deleteAuthor id_
        
        ("GET",    ["tags"])            -> withAuth AUser  query $ hdlGetAllEntities query getAllTags
        ("GET",    ["tags", id_])       -> withAuth AUser  query $ hdlGetEntity getTag id_
        ("POST",   ["tags"])            -> withAuth AAdmin query $ hdlPostEntity createTag body
        ("PUT",    ["tags", id_])       -> withAuth AAdmin query $ hdlPutEntity updateTag id_ body
        ("DELETE", ["tags", id_])       -> withAuth AAdmin query $ hdlDeleteEntity deleteTag id_

        ("GET",    ["categories"])      -> withAuth AUser  query $ hdlGetAllEntities query getAllCategories
        ("GET",    ["categories", id_]) -> withAuth AUser  query $ hdlGetEntity getCategory id_
        ("POST",   ["categories"])      -> withAuth AAdmin query $ hdlPostEntityEither createCategory body
        ("PUT",    ["categories", id_]) -> withAuth AAdmin query $ hdlPutEntity updateCategory id_ body
        ("DELETE", ["categories", id_]) -> withAuth AAdmin query $ hdlDeleteEntity deleteCategory id_

        ("GET",    ["drafts"])          -> withAuth AAuthor query $ hdlGetAllDrafts query
        ("GET",    ["drafts", id_])     -> withDraftAuth id_ query $ hdlGetEntity getDraft id_
        ("POST",   ["drafts"])          -> withAuth AAuthor query $ hdlPostDraft (lookup "user" query) body
        ("POST",   ["drafts", id_, "publish"]) -> withDraftAuth id_ query $ hdlPublishDraft id_
        ("PUT",    ["drafts", id_])     -> withDraftAuth id_ query $ hdlPutEntity updateDraft id_ body
        ("DELETE", ["drafts", id_])     -> withDraftAuth id_ query $ hdlDeleteEntity deleteDraft id_

        ("GET",    ["photos", id_])     -> withAuth AUser query $ hdlGetPhoto id_
        ("POST",   ["photos"])          -> withAuth AUser query $ hdlPostPhoto body

        ("GET",    ["posts", id_, "comments"])      -> withAuth AUser query $ hdlGetPostCommentaries query id_
        ("POST",   ["posts", id_, "comments"])      -> withAuth AUser query $ hdlPostCommentary (lookup "user" query) id_ body
        ("DELETE", ["posts", pId, "comments", cId]) -> withCommentAuth cId query $ hdlDeleteCommentary pId cId
        
        ("GET",    ["posts"])           -> withAuth AUser query $ hdlGetPostsFiltered query
        ("GET",    ["posts", id_])      -> withAuth AUser query $ hdlGetEntity getPost id_

        _ -> pure notFound
    let level = if statusIsSuccessful status
                then LevelInfo
                else LevelError
    logLn level $ show response
    send (renderResponse response)
