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
import Queries.Author
import Queries.Category
import Queries.Draft
import Queries.Post
import Queries.Tag
import Queries.User
import Queries.Util

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
         , UserT (DbQExpr s)
         , T2 s (Vector Int32) (Vector Text)
         , T2 s (Vector Int32) (Vector Text)
         , DbQExpr s (Vector Int32)))
  -> DbWith
       (DbQ s
         ( PostT (DbQExpr s)
         , UserT (DbQExpr s)
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
          postPublishedAt post ==. val_ date
        PfPublishedAtLt date ->
          postPublishedAt post <. val_ date
        PfPublishedAtGt date ->
          postPublishedAt post >. val_ date
        PfAuthorName firstName lastName ->
          userFirstName user ==. val_ firstName &&. userLastName user ==. val_ lastName
        PfCategoryId cId ->
          postCategoryId post ==. val_ cId
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
         , UserT (DbQExpr s)
         , T2 s (Vector Int32) (Vector Text)
         , T2 s (Vector Int32) (Vector Text)
         , DbQExpr s (Vector Int32)))
  -> DbWith
       (DbQ s
         ( PostT (DbQExpr s)
         , UserT (DbQExpr s)
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
  migration1 <- fromString <$> readFile "migrations/1.sql"
  migration2 <- fromString <$> readFile "migrations/2.sql"
  conn <- connectPostgreSQL "host='localhost' port='5432' dbname='haskell-news-server' user='postgres'"
  begin conn
  print =<< execute_ conn migration1
  print =<< execute_ conn migration2
  runBeamPostgresDebug putStrLn conn $ do
    runInsert $ insert (dbCategory newsDb) $
      insertValues
        [ Category 1 Nothing "Programming Languages"
        , Category 2 (Just 1) "Python"
        , Category 3 (Just 2) "A"
        , Category 4 (Just 2) "B"
        , Category 5 (Just 1) "C"
        , Category 6 Nothing "D"
        ]

    do xs <- runSelectReturningList $ selectWith withCategoryTree
       mapM_ (liftIO . putStrLn . show) xs

    do xs <- runSelectReturningList $ selectWith categoriesWithTrees
       mapM_ (liftIO . putStrLn . show) xs

    do xs <- runSelectReturningList $ selectWith postsWithCategories
       mapM_ (liftIO . putStrLn . show) xs

    runInsert $ insert (dbPhoto newsDb) $
      insertExpressions [Photo { photoId = default_, photoContent = val_ "" }]

    Right userId <- createUser CreateUser
                { cUserFirstName = "John"
                , cUserLastName = "Doe"
                , cUserAvatarId = 1
                , cUserIsAdmin = False
                }
    Right authorId <- createAuthor CreateAuthor
                        { cAuthorUserId = userId
                        , cAuthorShortDescription = ""
                        }
    do xs <- runSelectReturningList $ select $ all_ (dbUser newsDb)
       mapM_ (liftIO . putStrLn . show) xs

    tagId <- createTag CreateTag { cTagName = "A" }

    do x <- createDraft CreateDraft
              { cDraftShortName = ""
              , cDraftAuthorId = authorId
              , cDraftCategoryId = 1
              , cDraftTextContent = ""
              , cDraftMainPhotoId = 1
              , cDraftAdditionalPhotoIds = []
              , cDraftTagIds = [tagId]
              }
       liftIO $ print x
       case x of
         Left _ -> pure ()
         Right dId -> publishDraft dId >> pure ()

    do x <- deleteAuthor authorId
       liftIO $ print x
  rollback conn
