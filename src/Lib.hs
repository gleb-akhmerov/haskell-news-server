{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Data.ByteString ( ByteString )
import Data.Set ( Set )
import Data.String ( fromString )
import Data.Time ( UTCTime )

import Database.PostgreSQL.Simple ( connectPostgreSQL, execute_, begin, rollback, query_, Connection, fromBinary )
import Database.PostgreSQL.Simple.SqlQQ
import Database.Beam hiding (date)
import Database.Beam.Postgres

import BeamSchema hiding (Author)
import Queries

data PostFilter
  = PfPublishedAt UTCTime
  | PfPublishedAtLt UTCTime
  | PfPublishedAtGt UTCTime
  | PfTag Int
  | PfTagsIn [Int]
  | PfTagsAll [Int]
  | PfNameSubstring String
  | PfContentSubstring String
  deriving (Eq, Ord)

applyOneFilterToQuery :: PostFilter -> Q Postgres NewsDb s (PostT (QExpr Postgres s)) -> Q Postgres NewsDb s (PostT (QExpr Postgres s))
applyOneFilterToQuery flt query = do
  post <- query
  guard_ $
    case flt of
      PfPublishedAt date ->
        _postPublishedAt post ==. val_ date
      PfPublishedAtLt date ->
        _postPublishedAt post <. val_ date
      PfPublishedAtGt date ->
        _postPublishedAt post >. val_ date
      _ ->
        val_ True
  pure post

applyFiltersToQuery :: Set PostFilter -> Q Postgres NewsDb s (PostT (QExpr Postgres s)) -> Q Postgres NewsDb s (PostT (QExpr Postgres s))
applyFiltersToQuery filters query =
  foldr applyOneFilterToQuery query filters

data Order
  = Ascending
  | Descending

data Ordered a = Ordered Order a

data PostOrder
  = PoPublishedAt
  | PoAuthorName
  | PoCategoryName
  | PoPhotoCount

data User
  = User
    { uId :: Int
    , uFirstName :: String
    , uLastName :: String
    , uAvatar :: ByteString
    , uCreatedAt :: UTCTime
    , uIsAdmin :: Bool
    }

data Author
  = Author
    { aShortDescription :: String
    , aUser :: User
    }

getAllAuthors :: Connection -> IO [Author]
getAllAuthors conn = do
  rows <- query_ conn [sql|
    select
      a.short_description,
      u.id,
      u.first_name,
      u.last_name,
      u.avatar,
      u.created_at,
      u.is_admin
    from
      author a
    join
      usr u
    on
      u.id = a.id
  |]
  let f = \( aShortDescription
           , uId
           , uFirstName
           , uLastName
           , uAvatarBytea
           , uCreatedAt
           , uIsAdmin
           ) ->
             let uAvatar = fromBinary uAvatarBytea
                 aUser = User {..}
             in Author {..}
  return $ map f rows

data Request
  = GetPosts (Set PostFilter) (Ordered PostOrder)
  | CreateAuthor  -- admin
  | GetAllAuthors -- admin
  | UpdateAuthor  -- admin
  | DeleteAuthor  -- admin
  | CreateCategory -- admin
  | GetAllCategories
  | UpdateCategory -- admin
  | DeleteCategory -- admin
  | CreateTag -- admin
  | GetAllTags
  | UpdateTag -- admin
  | DeleteTag -- admin
  | CreateDraft  -- author
  | GetDraft     -- author
  | UpdateDraft  -- author
  | DeleteDraft  -- author
  | PublishDraft -- author
  | CreateUser
  | GetAllUsers
  | DeleteUser -- admin
  | CreateCommentary
  | GetPostCommentaries
  | DeleteCommentary

someFunc :: IO ()
someFunc = do
  migrationSql <- readFile "migrations/1.sql"
  conn <- connectPostgreSQL "host='localhost' port='5432' dbname='haskell-news-server' user='postgres'"
  begin conn
  print =<< execute_ conn (fromString migrationSql)
  runBeamPostgresDebug putStrLn conn $ runInsert $
    insert (_dbCategory newsDb) $
      insertValues
        [ Category 4 (CategoryId Nothing) "Programming Languages"
        , Category 5 (CategoryId (Just 4)) "Python"
        , Category 6 (CategoryId (Just 5)) "A"
        , Category 7 (CategoryId (Just 5)) "B"
        , Category 8 (CategoryId (Just 4)) "C"
        , Category 9 (CategoryId Nothing) "D"
        ]
  runBeamPostgresDebug putStrLn conn $ do
    cats <- runSelectReturningList $ select (categoryWithParents (val_ 7))
    mapM_ (liftIO . putStrLn . show) cats
  runBeamPostgresDebug putStrLn conn $ do
    xs <- runSelectReturningList $ select postsWithCategories
    mapM_ (liftIO . putStrLn . show) xs
  rollback conn
