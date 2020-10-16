{-# LANGUAGE OverloadedStrings #-}

module NewsSearchSpec where


import Control.Exception (bracket)
import qualified Data.Set as Set (fromList)
import Data.Int (Int32)
import Data.Text (Text)

import Database.Beam.Postgres
import Database.PostgreSQL.Simple (begin, rollback)
import Test.Hspec

import NewsServer.Lib (applyMigrations)
import NewsServer.Database.Author
import NewsServer.Database.Category
import NewsServer.Database.Draft
import NewsServer.Database.Photo
import NewsServer.Database.Post
import NewsServer.Database.Tag
import NewsServer.Database.User


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

makeCategory :: Text -> Pg Int32
makeCategory name = do
  Right categoryId <- createCategory CreateCategory
    { cCategoryParentId = Nothing
    , cCategoryName = name
    }
  return categoryId

makeTag :: Text -> Pg Int32
makeTag name = createTag CreateTag { cTagName = name }

makePost :: Int32 -> [Int32] -> Pg Int32
makePost categoryId tagIds = do
  photoId <- createPhoto ""
  Right userId <- createUser CreateUser
    { cUserFirstName = "John"
    , cUserLastName = "Doe"
    , cUserAvatarId = photoId
    }
  Right authorId <- createAuthor CreateAuthor
    { cAuthorUserId = userId
    , cAuthorShortDescription = ""
    }
  Right draftId <- createDraft authorId CreateDraft
    { cDraftShortName = "A draft"
    , cDraftCategoryId = categoryId
    , cDraftMainPhotoId = photoId
    , cDraftTextContent = "Draft text"
    , cDraftAdditionalPhotoIds = [photoId]
    , cDraftTagIds = tagIds
    }
  Right postId <- publishDraft draftId
  return postId

spec :: Spec
spec = do
  around withDatabaseConnection $ do

    describe "Post" $ do
      it "can be searched" $ \conn -> do
        let runPg = runBeamPostgres conn
        [cA, cB] <- runPg $ mapM makeCategory ["A", "B"]
        [tX, tY, tZ] <- runPg $ mapM makeTag ["X", "Y", "Z"]
        pA <- runPg $ makePost cA []
        pBYZ <- runPg $ makePost cB [tY, tZ]
        pAXY <- runPg $ makePost cA [tX, tY]
        do posts <- runPg $ getPosts (Set.fromList []) Nothing 1
           fmap rPostId posts `shouldBe` [pA, pBYZ, pAXY]
        do posts <- runPg $ getPosts (Set.fromList [PfCategoryId cA]) Nothing 1
           fmap rPostId posts `shouldBe` [pA, pAXY]
        do posts <- runPg $ getPosts (Set.fromList [PfCategoryId cB]) Nothing 1
           fmap rPostId posts `shouldBe` [pBYZ]
        do posts <- runPg $ getPosts (Set.fromList [PfTagId tX]) Nothing 1
           fmap rPostId posts `shouldBe` [pAXY]
        do posts <- runPg $ getPosts (Set.fromList [PfTagId tY]) Nothing 1
           fmap rPostId posts `shouldBe` [pBYZ, pAXY]
        do posts <- runPg $ getPosts (Set.fromList [PfTagId tZ]) Nothing 1
           fmap rPostId posts `shouldBe` [pBYZ]
        do posts <- runPg $ getPosts (Set.fromList [PfTagIdsIn [tX, tY, tZ]]) Nothing 1
           fmap rPostId posts `shouldBe` [pBYZ, pAXY]
        do posts <- runPg $ getPosts (Set.fromList [PfTagIdsAll [tX, tY]]) Nothing 1
           fmap rPostId posts `shouldBe` [pAXY]
        do posts <- runPg $ getPosts (Set.fromList [PfTagIdsAll [tX, tY, tZ]]) Nothing 1
           fmap rPostId posts `shouldBe` []
        do posts <- runPg $ getPosts (Set.fromList [PfTagId tY, PfCategoryId cB]) Nothing 1
           fmap rPostId posts `shouldBe` [pBYZ]
