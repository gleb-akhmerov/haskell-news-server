module NewsServer.Lib where


import qualified Data.Set as Set (fromList)
import Data.String (fromString)

import Database.PostgreSQL.Simple (execute_, begin, rollback)
import Database.Beam hiding (date)
import Database.Beam.Postgres

import NewsServer.Database.BeamSchema
import NewsServer.Database.Author
import NewsServer.Database.Category
import NewsServer.Database.Draft
import NewsServer.Database.Post
import NewsServer.Database.Tag
import NewsServer.Database.User


applyMigrations :: Connection -> IO ()
applyMigrations conn = do
  migration1 <- fromString <$> readFile "migrations/1.sql"
  migration2 <- fromString <$> readFile "migrations/2.sql"
  _ <- execute_ conn migration1
  _ <- execute_ conn migration2
  return ()

someFunc :: IO ()
someFunc = do
  conn <- connectPostgreSQL "host='localhost' port='5432' dbname='haskell-news-server' user='postgres'"
  begin conn
  applyMigrations conn
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
       mapM_ (liftIO . print) xs

    do xs <- runSelectReturningList $ select postsWithNestedEntities
       mapM_ (liftIO . print) xs

    runInsert $ insert (dbPhoto newsDb) $
      insertExpressions [Photo { photoId = default_, photoContent = val_ "" }]

    Right userId <- createUser CreateUser
                { cUserFirstName = "John"
                , cUserLastName = "Doe"
                , cUserAvatarId = 1
                }
    Right authorId <- createAuthor CreateAuthor
                        { cAuthorUserId = userId
                        , cAuthorShortDescription = ""
                        }
    do xs <- runSelectReturningList $ select $ all_ (dbUser newsDb)
       mapM_ (liftIO . print) xs

    tagId <- createTag CreateTag { cTagName = "A" }

    liftIO $ putStrLn "Publishing"

    do x <- createDraft authorId CreateDraft
              { cDraftShortName = ""
              , cDraftCategoryId = 1
              , cDraftTextContent = ""
              , cDraftMainPhotoId = 1
              , cDraftAdditionalPhotoIds = []
              , cDraftTagIds = [tagId]
              }
       liftIO $ print x
       case x of
         Left _ -> pure ()
         Right dId -> publishDraft dId >>= (liftIO . print)

    liftIO $ putStrLn "Published"

    do x <- deleteAuthor authorId
       liftIO $ print x

    do x <- getCategory 4
       liftIO $ print x

    do xs <- getAllCategories 1
       mapM_ (liftIO . print) xs

    do xs <- getPosts (Set.fromList []) Nothing 1
       mapM_ (liftIO . print) xs

    do xs <- getPosts (Set.fromList [PfTagIdsAll [tagId]]) (Just (PostOrder Descending PoPhotoCount)) 1
       mapM_ (liftIO . print) xs
  rollback conn
