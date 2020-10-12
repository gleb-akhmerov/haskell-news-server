{-# LANGUAGE OverloadedStrings #-}

module DatabaseSpec where


import Control.Exception (bracket)

import Database.Beam.Postgres
import Database.PostgreSQL.Simple (begin, rollback)
import Test.Hspec

import Lib (applyMigrations)
import Queries.Author
import Queries.Photo
import Queries.User


withDatabaseConnection :: (Connection -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection closeConnection
  where
    openConnection = do
      conn <- connect ConnectInfo
                { connectHost = "localhost"
                , connectPort = 5432
                , connectDatabase = "haskell-news-server"
                , connectUser = "postgres"
                , connectPassword = ""
                }
      begin conn
      applyMigrations conn
      return conn
    closeConnection = \conn -> do
      rollback conn
      close conn

spec :: Spec
spec =
  around withDatabaseConnection $ do

    describe "User" $ do
      it "can be created and deleted" $ \conn -> do
        _ <- runBeamPostgres conn $ do
          photoId <- createPhoto ""
          createUser CreateUser
            { cUserFirstName = "John"
            , cUserLastName = "Doe"
            , cUserAvatarId = photoId
            }
        users <- runBeamPostgres conn $ getAllUsers 1
        users `shouldBe`
          [ReturnedUser
             { rUserId = 1
             , rUserFirstName = "John"
             , rUserLastName = "Doe"
             , rUserAvatarId = 1
             , rUserIsAdmin = False
             , rUserCreatedAt = rUserCreatedAt (head users)
             }]

        _ <- runBeamPostgres conn $ deleteUser 1
        users2 <- runBeamPostgres conn $ getAllUsers 1
        users2 `shouldBe` []

    describe "Author" $ do
      it "can be created, updated and deleted" $ \conn -> do
        _ <- runBeamPostgres conn $ do
          photoId <- createPhoto ""
          Right userId <- createUser CreateUser
            { cUserFirstName = "John"
            , cUserLastName = "Doe"
            , cUserAvatarId = photoId
            }
          createAuthor CreateAuthor
            { cAuthorUserId = userId
            , cAuthorShortDescription = ""
            }
        authors <- runBeamPostgres conn $ getAllAuthors 1
        authors `shouldBe`
          [ReturnedAuthor
             { rAuthorId = 1
             , rAuthorShortDescription = ""
             }]

        _ <- runBeamPostgres conn $
          updateAuthor 1 UpdateAuthor
            { uAuthorNewShortDescription = "New description" }
        authors2 <- runBeamPostgres conn $ getAllAuthors 1
        authors2 `shouldBe`
          [ReturnedAuthor
             { rAuthorId = 1
             , rAuthorShortDescription = "New description"
             }]

        _ <- runBeamPostgres conn $ deleteAuthor 1
        authors3 <- runBeamPostgres conn $ getAllAuthors 1
        authors3 `shouldBe` []
