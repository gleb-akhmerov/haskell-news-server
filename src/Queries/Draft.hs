{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Queries.Draft where


import Control.Monad (unless)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Vector as Vector (fromList)

import Data.Aeson
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import BeamSchema
import Queries.Util


data CreateDraft = CreateDraft
  { cDraftShortName :: Text
  , cDraftAuthorId :: Int32
  , cDraftCategoryId :: Int32
  , cDraftTextContent :: Text
  , cDraftMainPhotoId :: Int32
  , cDraftAdditionalPhotoIds :: [Int32]
  , cDraftTagIds :: [Int32]
  }
  deriving (Generic, Show)

instance FromJSON CreateDraft where
  parseJSON = genericParseJSON defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop (length "cDraft") }

createDraft :: CreateDraft -> Pg (Either String Int32)
createDraft cd = runExceptT $ do
  makeSureEntityExists "Author" (dbAuthor newsDb) authorId (cDraftAuthorId cd)
  makeSureEntityExists "Category" (dbCategory newsDb) categoryId (cDraftCategoryId cd)
  makeSureEntityExists "Photo" (dbPhoto newsDb) photoId (cDraftMainPhotoId cd)
  makeSureTagsExist (cDraftTagIds cd)
  makeSurePhotosExist (cDraftAdditionalPhotoIds cd)

  [draft] <- runInsertReturningList $ insert (dbDraft newsDb) $
    insertExpressions
      [ Draft
          { draftId          = default_
          , draftShortName   = val_ (cDraftShortName cd)
          , draftCreatedAt   = now_
          , draftAuthorId    = val_ (cDraftAuthorId cd)
          , draftCategoryId  = val_ (cDraftCategoryId cd)
          , draftTextContent = val_ (cDraftTextContent cd)
          , draftMainPhotoId = val_ (cDraftMainPhotoId cd)
          }
      ]

  let additionalPhotoToRow pId =
        DraftAdditionalPhoto
          { draftAdditionalPhotoPhotoId = pId
          , draftAdditionalPhotoDraftId = draftId draft
          }
  runInsert $ insert (dbDraftAdditionalPhoto newsDb) $
    insertValues (map additionalPhotoToRow (cDraftAdditionalPhotoIds cd))

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

      pure (postId insertedPost)


data UpdateDraft = UpdateDraft
  { uDraftId :: Int32
  , uDraftNewShortName :: Maybe Text
  , uDraftNewAuthorId :: Maybe Int32
  , uDraftNewCategoryId :: Maybe Int32
  , uDraftNewTextContent :: Maybe Text
  , uDraftNewMainPhotoId :: Maybe Int32
  , uDraftNewAdditionalPhotoIds :: Maybe [Int32]
  , uDraftNewTagIds :: Maybe [Int32]
  }
  deriving (Generic, Show)

instance FromJSON UpdateDraft where
  parseJSON = genericParseJSON defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop (length "uDraft") }


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
      makeSureTagsExist newTagIds

  case uDraftNewMainPhotoId ud of
    Nothing ->
      pure ()
    Just newMainPhotoId ->
      makeSureEntityExists "Photo" (dbPhoto newsDb) photoId newMainPhotoId

  case uDraftNewAdditionalPhotoIds ud of
    Nothing ->
      pure ()
    Just newAdditionalPhotoIds ->
      makeSurePhotosExist newAdditionalPhotoIds

  runUpdate $ update (dbDraft newsDb)
                     (\d ->
                          maybeAssignment (uDraftNewShortName   ud) (\x -> draftShortName   d <-. val_ x)
                       <> maybeAssignment (uDraftNewAuthorId    ud) (\x -> draftAuthorId    d <-. val_ x)
                       <> maybeAssignment (uDraftNewCategoryId  ud) (\x -> draftCategoryId  d <-. val_ x)
                       <> maybeAssignment (uDraftNewTextContent ud) (\x -> draftTextContent d <-. val_ x)
                       <> maybeAssignment (uDraftNewMainPhotoId ud) (\x -> draftMainPhotoId d <-. val_ x))
                     (\d -> draftId d ==. val_ (uDraftId ud))

  case uDraftNewAdditionalPhotoIds ud of
    Nothing ->
      pure ()
    Just newAdditionalPhotoIds -> do
      runDelete $ delete (dbDraftAdditionalPhoto newsDb)
        (\dap -> draftAdditionalPhotoDraftId dap ==. val_ (uDraftId ud))
      let additionalPhotoToRow pId =
            DraftAdditionalPhoto
              { draftAdditionalPhotoPhotoId = pId
              , draftAdditionalPhotoDraftId = uDraftId ud
              }
      runInsert $ insert (dbDraftAdditionalPhoto newsDb) $
        insertValues (map additionalPhotoToRow newAdditionalPhotoIds)

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


makeSureTagsExist :: [Int32] -> ExceptT String Pg ()
makeSureTagsExist tagIds =
  unless (null tagIds) $ do
    tagIdsNotInDb <- runSelectReturningList $ select $ do
      cdTagId <- pgUnnestArray (val_ (Vector.fromList tagIds))
      tag <- leftJoin_ (all_ (dbTag newsDb)) (\t -> tagId t ==. cdTagId)
      guard_ (isNothing_ (tagId tag))
      pure cdTagId
    unless (null tagIdsNotInDb) $
      throwE $ "Tags with ids don't exist: " ++ show tagIdsNotInDb


makeSurePhotosExist :: [Int32] -> ExceptT String Pg ()
makeSurePhotosExist photoIds =
  unless (null photoIds) $ do
    photoIdsNotInDb <- runSelectReturningList $ select $ do
      cdAdditionalPhotoId <- pgUnnestArray (val_ (Vector.fromList photoIds))
      photo <- leftJoin_ (all_ (dbPhoto newsDb)) (\p -> photoId p ==. cdAdditionalPhotoId)
      guard_ (isNothing_ (photoId photo))
      pure cdAdditionalPhotoId
    unless (null photoIdsNotInDb) $
      throwE $ "Photos with ids don't exist: " ++ show photoIdsNotInDb
