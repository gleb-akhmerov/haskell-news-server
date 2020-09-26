{-# LANGUAGE OverloadedStrings #-}

module Lib where


import qualified Data.Set as Set (fromList)
import Data.String (fromString)

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

    do xs <- runSelectReturningList $ selectWith $
               filterAndSortPosts (Set.fromList [PfSearchSubstring "AAA"]) (PostOrder Descending PoPhotoCount)
       liftIO $ print xs
  rollback conn
