{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Data.Int (Int32)
import Data.Set (Set)
import Data.String (fromString)
import Data.Text (Text)
import Data.Time (LocalTime)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)

import Database.PostgreSQL.Simple (execute_, begin, rollback)
import Database.Beam hiding (date)
import Database.Beam.Postgres

import BeamSchema
import Queries

data PostFilter
  = PfPublishedAt LocalTime
  | PfPublishedAtLt LocalTime
  | PfPublishedAtGt LocalTime
  | PfAuthorName Text Text
  | PfCategoryId Int32
  | PfTagId Int32
  | PfTagIdsIn [Int32]
  | PfTagIdsAll [Int32]
  | PfNameSubstring Text
  | PfContentSubstring Text
  deriving (Eq, Ord)

applyOneFilterToQuery
  :: PostFilter
  -> DbWith
       (DbQ s
         ( PostT (DbQExpr s)
         , UsrT (DbQExpr s)
         , T2 s (Vector Int32) (Vector Text)
         , T2 s (Vector Int32) (Vector Text)
         , DbQExpr s (Vector Int32)))
  -> DbWith
       (DbQ s
         ( PostT (DbQExpr s)
         , UsrT (DbQExpr s)
         , T2 s (Vector Int32) (Vector Text)
         , T2 s (Vector Int32) (Vector Text)
         , DbQExpr s (Vector Int32)))
applyOneFilterToQuery flt withQuery = do
  postQuery <- withQuery
  pure $ do
    row@(post, user, (_catIds, _catNames), (tagIds, _tagNames), _additionalPhotoIds) <- postQuery
    guard_ $
      case flt of
        PfPublishedAt date ->
          _postPublishedAt post ==. val_ date
        PfPublishedAtLt date ->
          _postPublishedAt post <. val_ date
        PfPublishedAtGt date ->
          _postPublishedAt post >. val_ date
        PfAuthorName firstName lastName ->
          _usrFirstName user ==. val_ firstName &&. _usrLastName user ==. val_ lastName
        PfCategoryId cId ->
          _postCategoryId post ==. val_ (CategoryId cId)
        PfTagId tId ->
          val_ (Vector.fromList [tId]) `isSubsetOf_` tagIds
        PfTagIdsIn tIds ->
          val_ (Vector.fromList tIds) `isSubsetOf_` tagIds
        PfTagIdsAll tIds ->
          val_ (Vector.fromList tIds) `isSupersetOf_` tagIds
        _ ->
          val_ True
    pure row

applyFiltersToQuery
  :: Set PostFilter
  -> DbWith
       (DbQ s
         ( PostT (DbQExpr s)
         , UsrT (DbQExpr s)
         , T2 s (Vector Int32) (Vector Text)
         , T2 s (Vector Int32) (Vector Text)
         , DbQExpr s (Vector Int32)))
  -> DbWith
       (DbQ s
         ( PostT (DbQExpr s)
         , UsrT (DbQExpr s)
         , T2 s (Vector Int32) (Vector Text)
         , T2 s (Vector Int32) (Vector Text)
         , DbQExpr s (Vector Int32)))
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
    do xs <- runSelectReturningList $ selectWith withCategoryTree
       mapM_ (liftIO . putStrLn . show) xs

    do xs <- runSelectReturningList $ selectWith categoriesWithTrees
       mapM_ (liftIO . putStrLn . show) xs

    do xs <- runSelectReturningList $ selectWith postsWithCategories
       mapM_ (liftIO . putStrLn . show) xs

    createUser CreateUser
                 { cUserFirstName = "John"
                 , cUserLastName = "Doe"
                 , cUserAvatar = ""
                 , cUserIsAdmin = False
                 }
    do xs <- runSelectReturningList $ select $ all_ (_dbUsr newsDb)
       mapM_ (liftIO . putStrLn . show) xs

    runInsert $ insert (_dbPhoto newsDb) $
      insertExpressions
        [ Photo
            { _photoId      = default_
            , _photoContent = val_ ""
            }
        ]
    do xs <- runSelectReturningList $ select $ all_ (_dbPhoto newsDb)
       mapM_ (liftIO . print) xs
    deleteOrphanedPhotos
    do xs <- runSelectReturningList $ select $ all_ (_dbPhoto newsDb)
       mapM_ (liftIO . print) xs
  rollback conn
