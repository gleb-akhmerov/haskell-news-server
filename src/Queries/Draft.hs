{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Queries.Draft where


import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Vector as Vector (fromList)

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import BeamSchema
import Queries.Photo
import Queries.Util


data CreateDraft = CreateDraft
  { cDraftShortName :: Text
  , cDraftAuthorId :: Int32
  , cDraftCategoryId :: Int32
  , cDraftTextContent :: Text
  , cDraftMainPhoto :: ByteString
  , cDraftAdditionalPhotos :: [ByteString]
  , cDraftTagIds :: [Int32]
  }

createDraft :: CreateDraft -> Pg (Either String Int32)
createDraft cd = runExceptT $ do
  makeSureEntityExists "Author" (dbAuthor newsDb) authorId (cDraftAuthorId cd)
  makeSureEntityExists "Category" (dbCategory newsDb) categoryId (cDraftCategoryId cd)
  unless (null (cDraftTagIds cd)) $ do
    tagIdsNotInDb <- runSelectReturningList $ select $ do
      cdTagId <- pgUnnestArray (val_ (Vector.fromList (cDraftTagIds cd)))
      tag <- leftJoin_ (all_ (dbTag newsDb)) (\t -> tagId t ==. cdTagId)
      guard_ (isNothing_ (tagId tag))
      pure cdTagId
    unless (null tagIdsNotInDb) $
      throwE $ "Tags with ids don't exist: " ++ show tagIdsNotInDb

  let photoToRow p =
        Photo
          { photoId      = default_
          , photoContent = val_ p
          }
  [mainPhoto] <- runInsertReturningList $ insert (dbPhoto newsDb) $
    insertExpressions [photoToRow (cDraftMainPhoto cd)]
  additionalPhotos <- runInsertReturningList $ insert (dbPhoto newsDb) $
    insertExpressions (map photoToRow (cDraftAdditionalPhotos cd))

  [draft] <- runInsertReturningList $ insert (dbDraft newsDb) $
    insertExpressions
      [ Draft
          { draftId          = default_
          , draftShortName   = val_ (cDraftShortName cd)
          , draftCreatedAt   = now_
          , draftAuthorId    = val_ (cDraftAuthorId cd)
          , draftCategoryId  = val_ (cDraftCategoryId cd)
          , draftTextContent = val_ (cDraftTextContent cd)
          , draftMainPhotoId = val_ (photoId mainPhoto)
          }
      ]

  let additionalPhotoToRow p =
        DraftAdditionalPhoto
          { draftAdditionalPhotoPhotoId = photoId p
          , draftAdditionalPhotoDraftId = draftId draft
          }
  runInsert $ insert (dbDraftAdditionalPhoto newsDb) $
    insertValues (map additionalPhotoToRow additionalPhotos)

  let tagToRow tId =
        DraftTag
          { draftTagTagId   = tId
          , draftTagDraftId = draftId draft
          }
  runInsert $ insert (dbDraftTag newsDb) $
    insertValues (map tagToRow (cDraftTagIds cd))

  pure (draftId draft)


publishDraft :: Int32 -> Pg (Either String Int32)
publishDraft dId = runExceptT $ do
  mDraft <- runSelectReturningOne $ select $
    filter_ (\d -> draftId d ==. val_ dId) (all_ (dbDraft newsDb))
  case mDraft of
    Nothing ->
      throwE $ "Draft with id doesn't exist: " ++ show dId
    Just draft -> do
      let newPost :: PostT (QExpr Postgres s)
          newPost =
            Post
              { postId          = val_ (draftId draft)
              , postShortName   = val_ (draftShortName draft)
              , postPublishedAt = now_
              , postAuthorId    = val_ (draftAuthorId draft)
              , postCategoryId  = val_ (draftCategoryId draft)
              , postTextContent = val_ (draftTextContent draft)
              , postMainPhotoId = val_ (draftMainPhotoId draft)
              }
      [insertedPost] <- runInsertReturningList $ insertOnConflict (dbPost newsDb)
        (insertExpressions [newPost])
        (conflictingFields postId)
        (onConflictUpdateSet (\fields _oldValues -> fields <-. newPost))

      runDelete $ delete (dbPostTag newsDb)
        (\pt -> postTagPostId pt ==. val_ (postId insertedPost))
      draftTags <- runSelectReturningList $ select $
        join_ (dbDraftTag newsDb) (\dt -> draftTagDraftId dt ==. val_ (draftId draft))
      let draftTagToRow draftTag =
            PostTag
              { postTagTagId  = draftTagTagId draftTag
              , postTagPostId = draftId draft
              }
      runInsert $ insert (dbPostTag newsDb) $
        insertValues (map draftTagToRow draftTags)

      runDelete $ delete (dbPostAdditionalPhoto newsDb)
        (\pap -> postAdditionalPhotoPostId pap ==. val_ (postId insertedPost))
      draftPhotos <- runSelectReturningList $ select $
        join_ (dbDraftAdditionalPhoto newsDb) (\dap -> draftAdditionalPhotoDraftId dap ==. (val_ (draftId draft)))
      let draftAdditionalPhotoToRow dap =
            PostAdditionalPhoto
              { postAdditionalPhotoPhotoId = draftAdditionalPhotoPhotoId dap
              , postAdditionalPhotoPostId  = draftId draft
              }
      runInsert $ insert (dbPostAdditionalPhoto newsDb) $
        insertValues (map draftAdditionalPhotoToRow draftPhotos)

      lift $ deleteOrphanedPhotos
      
      pure (postId insertedPost)


data UpdateDraft = UpdateDraft
  { uDraftId :: Int32
  , uDraftNewShortName :: Maybe Text
  , uDraftNewAuthorId :: Maybe Int32
  , uDraftNewCategoryId :: Maybe Int32
  , uDraftNewTextContent :: Maybe Text
  , uDraftNewMainPhoto :: Maybe ByteString
  , uDraftNewAdditionalPhotos :: Maybe [ByteString]
  , uDraftNewTagIds :: Maybe [Int32]
  }


updateDraft :: UpdateDraft -> Pg (Either String ())
updateDraft ud = runExceptT $ do
  makeSureEntityExists "Draft" (dbDraft newsDb) draftId (uDraftId ud)

  case uDraftNewAuthorId ud of
    Nothing ->
      pure ()
    Just newAuthorId ->
      makeSureEntityExists "Author" (dbAuthor newsDb) authorId newAuthorId

  case uDraftNewCategoryId ud of
    Nothing ->
      pure ()
    Just newCategoryId ->
      makeSureEntityExists "Category" (dbCategory newsDb) categoryId newCategoryId

  case uDraftNewTagIds ud of
    Nothing ->
      pure ()
    Just newTagIds ->
      unless (null newTagIds) $ do
        tagIdsNotInDb <- runSelectReturningList $ select $ do
          cdTagId <- pgUnnestArray (val_ (Vector.fromList newTagIds))
          tag <- leftJoin_ (all_ (dbTag newsDb)) (\t -> tagId t ==. cdTagId)
          guard_ (isNothing_ (tagId tag))
          pure cdTagId
        unless (null tagIdsNotInDb) $
          throwE $ "Tags with ids don't exist: " ++ show tagIdsNotInDb

  let photoToRow p =
        Photo
          { photoId      = default_
          , photoContent = val_ p
          }

  mMainPhoto <- case uDraftNewMainPhoto ud of
    Nothing ->
      pure Nothing
    Just newMainPhoto -> do
      [mainPhoto] <- runInsertReturningList $ insert (dbPhoto newsDb) $
        insertExpressions [photoToRow newMainPhoto]
      pure (Just mainPhoto)

  runUpdate $ update (dbDraft newsDb)
                     (\d ->
                          maybeAssignment (uDraftNewShortName   ud) (\x -> draftShortName   d <-. val_ x)
                       <> maybeAssignment (uDraftNewAuthorId    ud) (\x -> draftAuthorId    d <-. val_ x)
                       <> maybeAssignment (uDraftNewCategoryId  ud) (\x -> draftCategoryId  d <-. val_ x)
                       <> maybeAssignment (uDraftNewTextContent ud) (\x -> draftTextContent d <-. val_ x)
                       <> maybeAssignment (fmap photoId mMainPhoto) (\x -> draftMainPhotoId d <-. val_ x))
                     (\d -> draftId d ==. val_ (uDraftId ud))

  case uDraftNewAdditionalPhotos ud of
    Nothing ->
      pure ()
    Just newAdditionalPhotos -> do
      runDelete $ delete (dbDraftAdditionalPhoto newsDb)
        (\dap -> draftAdditionalPhotoDraftId dap ==. val_ (uDraftId ud))
      additionalPhotos <- runInsertReturningList $ insert (dbPhoto newsDb) $
        insertExpressions (map photoToRow newAdditionalPhotos)
      let additionalPhotoToRow p =
            DraftAdditionalPhoto
              { draftAdditionalPhotoPhotoId = photoId p
              , draftAdditionalPhotoDraftId = uDraftId ud
              }
      runInsert $ insert (dbDraftAdditionalPhoto newsDb) $
        insertValues (map additionalPhotoToRow additionalPhotos)

  case uDraftNewTagIds ud of
    Nothing ->
      pure ()
    Just newTagIds -> do
      runDelete $ delete (dbDraftTag newsDb)
        (\dt -> draftTagDraftId dt ==. val_ (uDraftId ud))
      let tagToRow tId =
            DraftTag
              { draftTagTagId   = tId
              , draftTagDraftId = uDraftId ud
              }
      runInsert $ insert (dbDraftTag newsDb) $
        insertValues (map tagToRow newTagIds)

  when (isJust (uDraftNewMainPhoto ud) || isJust (uDraftNewAdditionalPhotos ud)) $
    lift $ deleteOrphanedPhotos
