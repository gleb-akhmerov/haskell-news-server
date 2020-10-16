{-# LANGUAGE OverloadedStrings #-}

module DatabaseSpec where


import Control.Exception (bracket)
import qualified Data.Set as Set (fromList)

import Database.Beam.Postgres
import Database.PostgreSQL.Simple (begin, rollback)
import Test.Hspec

import NewsServer.Lib (applyMigrations)
import NewsServer.MaybeOrUnspecified
import NewsServer.Database.Author
import NewsServer.Database.Category
import NewsServer.Database.Commentary
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

spec :: Spec
spec = do
  around withDatabaseConnection $ do

    describe "Photo" $ do
      it "can be created" $ \conn -> do
        _ <- runBeamPostgres conn (createPhoto "")
        runBeamPostgres conn (getPhoto 1) `shouldReturn` Just ""

    describe "User" $ do
      it "can be created and deleted" $ \conn -> do
        _ <- runBeamPostgres conn $ do
          photoId <- createPhoto ""
          createUser CreateUser
            { cUserFirstName = "John"
            , cUserLastName = "Doe"
            , cUserAvatarId = photoId
            }
        users <- runBeamPostgres conn (getAllUsers 1)
        users `shouldBe`
          [ReturnedUser
             { rUserId = 1
             , rUserFirstName = "John"
             , rUserLastName = "Doe"
             , rUserAvatarId = 1
             , rUserIsAdmin = False
             , rUserCreatedAt = rUserCreatedAt (head users)
             }]

        _ <- runBeamPostgres conn (deleteUser 1)
        runBeamPostgres conn (getAllUsers 1) `shouldReturn` []

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
        runBeamPostgres conn (getAllAuthors 1)
          `shouldReturn`
          [ReturnedAuthor
             { rAuthorId = 1
             , rAuthorShortDescription = ""
             }]

        _ <- runBeamPostgres conn $
          updateAuthor 1 UpdateAuthor
            { uAuthorNewShortDescription = "New description" }
        runBeamPostgres conn (getAllAuthors 1)
          `shouldReturn`
          [ReturnedAuthor
             { rAuthorId = 1
             , rAuthorShortDescription = "New description"
             }]

        _ <- runBeamPostgres conn (deleteAuthor 1)
        runBeamPostgres conn (getAllAuthors 1) `shouldReturn` []

    describe "Category" $ do
      it "can be created, updated and deleted" $ \conn -> do
        _ <- runBeamPostgres conn $ do
          createCategory CreateCategory
            { cCategoryParentId = Nothing
            , cCategoryName = "A category"
            }
        runBeamPostgres conn (getAllCategories 1)
          `shouldReturn`
          [ReturnedCategory
             { rCategoryId = 1
             , rCategoryParent = Nothing
             , rCategoryName = "A category"
             }]

        _ <- runBeamPostgres conn $
          updateCategory 1 UpdateCategory
            { uCategoryNewParentId = Unspecified
            , uCategoryNewName = Just "New name"
            }
        runBeamPostgres conn (getAllCategories 1)
          `shouldReturn`
          [ReturnedCategory
             { rCategoryId = 1
             , rCategoryParent = Nothing
             , rCategoryName = "New name"
             }]

        _ <- runBeamPostgres conn (deleteCategory 1)
        runBeamPostgres conn (getAllCategories 1) `shouldReturn` []

    describe "Tag" $ do
      it "can be created, updated and deleted" $ \conn -> do
        _ <- runBeamPostgres conn $ do
          createTag CreateTag { cTagName = "A tag" }
        runBeamPostgres conn (getAllTags 1)
          `shouldReturn`
          [ReturnedTag
             { rTagId = 1
             , rTagName = "A tag"
             }]

        _ <- runBeamPostgres conn $
          updateTag 1 UpdateTag { uTagNewName = "New name" }
        runBeamPostgres conn (getAllTags 1)
          `shouldReturn`
          [ReturnedTag
             { rTagId = 1
             , rTagName = "New name"
             }]

        _ <- runBeamPostgres conn (deleteTag 1)
        runBeamPostgres conn (getAllTags 1) `shouldReturn` []

    describe "Draft" $ do
      it "can be created, updated and deleted" $ \conn -> do
        photoId <- runBeamPostgres conn (createPhoto "")
        Right authorId <- runBeamPostgres conn $ do
          Right userId <- createUser CreateUser
            { cUserFirstName = "John"
            , cUserLastName = "Doe"
            , cUserAvatarId = photoId
            }
          createAuthor CreateAuthor
            { cAuthorUserId = userId
            , cAuthorShortDescription = ""
            }
        Right categoryId <- runBeamPostgres conn $
          createCategory CreateCategory
            { cCategoryParentId = Nothing
            , cCategoryName = "A category"
            }
        _ <- runBeamPostgres conn $ do
          createDraft authorId CreateDraft
            { cDraftShortName = "A draft"
            , cDraftCategoryId = categoryId
            , cDraftMainPhotoId = photoId
            , cDraftTextContent = "Draft text"
            , cDraftAdditionalPhotoIds = [photoId]
            , cDraftTagIds = []
            }
        drafts <- runBeamPostgres conn (getAllDrafts authorId 1)
        drafts `shouldBe`
          [ReturnedDraft
             { rDraftId = 1
             , rDraftShortName = "A draft"
             , rDraftCreatedAt = rDraftCreatedAt (head drafts)
             , rDraftAuthorId = authorId
             , rDraftCategoryId = categoryId
             , rDraftTextContent = "Draft text"
             , rDraftMainPhotoId = photoId
             }]

        _ <- runBeamPostgres conn $
          updateDraft 1 UpdateDraft
            { uDraftNewShortName = Just "New name"
            , uDraftNewCategoryId = Nothing
            , uDraftNewTextContent = Just "New text content"
            , uDraftNewMainPhotoId = Nothing
            , uDraftNewAdditionalPhotoIds = Nothing
            , uDraftNewTagIds = Nothing
            }
        drafts2 <- runBeamPostgres conn (getAllDrafts authorId 1)
        drafts2 `shouldBe`
          [ReturnedDraft
             { rDraftId = 1
             , rDraftShortName = "New name"
             , rDraftCreatedAt = rDraftCreatedAt (head drafts)
             , rDraftAuthorId = authorId
             , rDraftCategoryId = categoryId
             , rDraftTextContent = "New text content"
             , rDraftMainPhotoId = photoId
             }]

        _ <- runBeamPostgres conn (deleteDraft 1)
        runBeamPostgres conn (getAllDrafts authorId 1) `shouldReturn` []

    describe "Post" $ do
      it "can be published from draft and deleted" $ \conn -> do
        photoId <- runBeamPostgres conn (createPhoto "")
        Right authorId <- runBeamPostgres conn $ do
          Right userId <- createUser CreateUser
            { cUserFirstName = "John"
            , cUserLastName = "Doe"
            , cUserAvatarId = photoId
            }
          createAuthor CreateAuthor
            { cAuthorUserId = userId
            , cAuthorShortDescription = ""
            }
        Right categoryId <- runBeamPostgres conn $
          createCategory CreateCategory
            { cCategoryParentId = Nothing
            , cCategoryName = "A category"
            }
        Right draftId <- runBeamPostgres conn $ do
          createDraft authorId CreateDraft
            { cDraftShortName = "A draft"
            , cDraftCategoryId = categoryId
            , cDraftMainPhotoId = photoId
            , cDraftTextContent = "Draft text"
            , cDraftAdditionalPhotoIds = [photoId]
            , cDraftTagIds = []
            }
        _ <- runBeamPostgres conn (publishDraft draftId)
        posts <- runBeamPostgres conn (getPosts (Set.fromList []) Nothing 1)
        posts `shouldBe`
          [ReturnedPost
            { rPostId = draftId
            , rPostShortName = "A draft"
            , rPostPublishedAt = rPostPublishedAt (head posts)
            , rPostAuthor =
                ReturnedPostAuthor
                  { rPostAuthorId = authorId
                  , rPostAuthorFirstName = "John"
                  , rPostAuthorLastName = "Doe"
                  , rPostAuthorAvatarId = photoId
                  , rPostAuthorIsAdmin = False
                  , rPostAuthorShortDescription = ""
                  }
            , rPostCategory =
                ReturnedCategory
                  { rCategoryId = categoryId
                  , rCategoryParent = Nothing
                  , rCategoryName = "A category"
                  }
            , rPostTags = []
            , rPostTextContent = "Draft text"
            , rPostMainPhotoId = photoId
            , rPostAdditionalPhotoIds = [photoId]
            }]

        _ <- runBeamPostgres conn $
          updateDraft draftId UpdateDraft
            { uDraftNewShortName = Just "New name"
            , uDraftNewCategoryId = Nothing
            , uDraftNewTextContent = Just "New text content"
            , uDraftNewMainPhotoId = Nothing
            , uDraftNewAdditionalPhotoIds = Just []
            , uDraftNewTagIds = Nothing
            }
        _ <- runBeamPostgres conn (publishDraft draftId)
        posts2 <- runBeamPostgres conn (getPosts (Set.fromList []) Nothing 1)
        posts2 `shouldBe`
          [ReturnedPost
            { rPostId = draftId
            , rPostShortName = "New name"
            , rPostPublishedAt = rPostPublishedAt (head posts)
            , rPostAuthor =
                ReturnedPostAuthor
                  { rPostAuthorId = authorId
                  , rPostAuthorFirstName = "John"
                  , rPostAuthorLastName = "Doe"
                  , rPostAuthorAvatarId = photoId
                  , rPostAuthorIsAdmin = False
                  , rPostAuthorShortDescription = ""
                  }
            , rPostCategory =
                ReturnedCategory
                  { rCategoryId = categoryId
                  , rCategoryParent = Nothing
                  , rCategoryName = "A category"
                  }
            , rPostTags = []
            , rPostTextContent = "New text content"
            , rPostMainPhotoId = photoId
            , rPostAdditionalPhotoIds = []
            }]

    describe "Commentary" $ do
      it "can be added to a post and deleted" $ \conn -> do
        photoId <- runBeamPostgres conn (createPhoto "")
        Right authorId <- runBeamPostgres conn $ do
          Right userId <- createUser CreateUser
            { cUserFirstName = "John"
            , cUserLastName = "Doe"
            , cUserAvatarId = photoId
            }
          createAuthor CreateAuthor
            { cAuthorUserId = userId
            , cAuthorShortDescription = ""
            }
        Right categoryId <- runBeamPostgres conn $
          createCategory CreateCategory
            { cCategoryParentId = Nothing
            , cCategoryName = "A category"
            }
        Right draftId <- runBeamPostgres conn $ do
          createDraft authorId CreateDraft
            { cDraftShortName = "A draft"
            , cDraftCategoryId = categoryId
            , cDraftMainPhotoId = photoId
            , cDraftTextContent = "Draft text"
            , cDraftAdditionalPhotoIds = [photoId]
            , cDraftTagIds = []
            }
        Right postId <- runBeamPostgres conn (publishDraft draftId)
        Right commentId <- runBeamPostgres conn $
          createCommentary authorId postId CreateCommentary
            { cCommentaryContent = "Commentary content" }
        runBeamPostgres conn (getPostCommentaries postId 1)
          `shouldReturn`
            Just [ReturnedCommentary
                    { rCommentaryId = commentId
                    , rCommentaryUserId = authorId
                    , rCommentaryPostId = postId
                    , rCommentaryContent = "Commentary content"
                    }]

        _ <- runBeamPostgres conn (deleteCommentary postId commentId)
        runBeamPostgres conn (getPostCommentaries postId 1)
          `shouldReturn` Just []
