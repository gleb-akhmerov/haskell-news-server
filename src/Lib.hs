{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Data.Set (Set)
import Data.String (fromString)
import Data.Time (LocalTime)

import Database.PostgreSQL.Simple (execute_, begin, rollback)
import Database.Beam hiding (date)
import Database.Beam.Postgres

import BeamSchema
import Queries

data PostFilter
  = PfPublishedAt LocalTime
  | PfPublishedAtLt LocalTime
  | PfPublishedAtGt LocalTime
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

data PostOrder = PostOrder Order PostOrderBy

data PostOrderBy
  = PoPublishedAt
  | PoAuthorName
  | PoCategoryName
  | PoPhotoCount

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

    xs <- runSelectReturningList $ select postsWithCategories
    mapM_ (liftIO . putStrLn . show) xs

    createUser CreateUser
                 { cUserFirstName = "John"
                 , cUserLastName = "Doe"
                 , cUserAvatar = ""
                 , cUserIsAdmin = False
                 }
    us <- runSelectReturningList $ select $ all_ (_dbUsr newsDb)
    mapM_ (liftIO . putStrLn . show) us

    runInsert $ insert (_dbPhoto newsDb) $
      insertExpressions
        [ Photo
            { _photoId      = default_
            , _photoContent = val_ ""
            }
        ]
    ps <- runSelectReturningList $ select $ all_ (_dbPhoto newsDb)
    mapM_ (liftIO . print) ps
    deleteOrphanedPhotos
    ps <- runSelectReturningList $ select $ all_ (_dbPhoto newsDb)
    mapM_ (liftIO . print) ps
  rollback conn
