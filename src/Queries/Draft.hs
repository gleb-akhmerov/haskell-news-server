{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Queries.Draft where


import Control.Monad (unless)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Function ((&))
import Data.Int (Int32)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time (LocalTime)
import qualified Data.Vector as Vector (fromList)

import Data.Aeson
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import BeamSchema
import Queries.Util


data CreateDraft = CreateDraft
  { cDraftShortName :: Text
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

createDraft :: Int32 -> CreateDraft -> Pg (Either String Int32)
createDraft cDraftAuthorId cd = runExceptT $ do
  makeSureEntityExists "Author" (dbAuthor newsDb) authorId cDraftAuthorId
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
          , draftAuthorId    = val_ cDraftAuthorId
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
  { uDraftNewShortName :: Maybe Text
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


updateDraft :: Int32 -> UpdateDraft -> Pg (Either String ())
updateDraft uDraftId ud = runExceptT $ do
  makeSureEntityExists "Draft" (dbDraft newsDb) draftId uDraftId
  maybeDo (makeSureEntityExists "Category" (dbCategory newsDb) categoryId) (uDraftNewCategoryId ud)
  maybeDo makeSureTagsExist (uDraftNewTagIds ud)
  maybeDo (makeSureEntityExists "Photo" (dbPhoto newsDb) photoId) (uDraftNewMainPhotoId ud)
  maybeDo makeSurePhotosExist (uDraftNewAdditionalPhotoIds ud)

  runUpdate $ update (dbDraft newsDb)
                     (\d ->
                          maybeAssignment (uDraftNewShortName   ud) (\x -> draftShortName   d <-. val_ x)
                       <> maybeAssignment (uDraftNewCategoryId  ud) (\x -> draftCategoryId  d <-. val_ x)
                       <> maybeAssignment (uDraftNewTextContent ud) (\x -> draftTextContent d <-. val_ x)
                       <> maybeAssignment (uDraftNewMainPhotoId ud) (\x -> draftMainPhotoId d <-. val_ x))
                     (\d -> draftId d ==. val_ uDraftId)

  case uDraftNewAdditionalPhotoIds ud of
    Nothing ->
      pure ()
    Just newAdditionalPhotoIds -> do
      runDelete $ delete (dbDraftAdditionalPhoto newsDb)
        (\dap -> draftAdditionalPhotoDraftId dap ==. val_ uDraftId)
      let additionalPhotoToRow pId =
            DraftAdditionalPhoto
              { draftAdditionalPhotoPhotoId = pId
              , draftAdditionalPhotoDraftId = uDraftId
              }
      runInsert $ insert (dbDraftAdditionalPhoto newsDb) $
        insertValues (map additionalPhotoToRow newAdditionalPhotoIds)

  case uDraftNewTagIds ud of
    Nothing ->
      pure ()
    Just newTagIds -> do
      runDelete $ delete (dbDraftTag newsDb)
        (\dt -> draftTagDraftId dt ==. val_ uDraftId)
      let tagToRow tId =
            DraftTag
              { draftTagTagId   = tId
              , draftTagDraftId = uDraftId
              }
      runInsert $ insert (dbDraftTag newsDb) $
        insertValues (map tagToRow newTagIds)


deleteDraft :: Int32 -> Pg (Either String ())
deleteDraft dDraftId = runExceptT $ do
  makeSureEntityExists "Draft" (dbDraft newsDb) draftId dDraftId
  makeSureNoReferenceExists "Draft" "Posts" (dbPost newsDb) postId postId dDraftId
  runDelete $
    delete (dbDraftTag newsDb)
      (\dt -> draftTagDraftId dt ==. val_ dDraftId)
  runDelete $
    delete (dbDraftAdditionalPhoto newsDb)
      (\dt -> draftAdditionalPhotoDraftId dt ==. val_ dDraftId)
  runDelete $
    delete (dbDraft newsDb)
      (\d -> draftId d ==. val_ dDraftId)


data ReturnedDraft = ReturnedDraft
  { rDraftId          :: Int32
  , rDraftShortName   :: Text
  , rDraftCreatedAt   :: LocalTime
  , rDraftAuthorId    :: Int32
  , rDraftCategoryId  :: Int32
  , rDraftTextContent :: Text
  , rDraftMainPhotoId :: Int32
  }
  deriving (Generic, Show)

instance ToJSON ReturnedDraft where
  toJSON = genericToJSON defaultOptions
             { fieldLabelModifier = camelTo2 '_' . drop (length "rDraft") }

draftToReturned :: Draft -> ReturnedDraft
draftToReturned d =
  ReturnedDraft
    { rDraftId = draftId d
    , rDraftShortName = draftShortName d
    , rDraftCreatedAt = draftCreatedAt d
    , rDraftAuthorId = draftAuthorId d
    , rDraftCategoryId = draftCategoryId d
    , rDraftTextContent = draftTextContent d
    , rDraftMainPhotoId = draftMainPhotoId d
    }

getDraft :: Int32 -> Pg (Maybe ReturnedDraft)
getDraft gDraftId = do
  mDraft <- runSelectReturningOne $ select $
              filter_ (\a -> draftId a ==. val_ gDraftId)
                      (all_ (dbDraft newsDb))
  pure (fmap draftToReturned mDraft)

getAllDrafts :: Integer -> Pg [ReturnedDraft]
getAllDrafts pageNum = do
  drafts <- runSelectReturningList $ select $
              all_ (dbDraft newsDb)
              & orderBy_ (asc_ . draftId)
              & offset_ (20 * (pageNum - 1))
              & limit_ 20
  pure (fmap draftToReturned drafts)


isDraftByAuthor :: Int32 -> Int32 -> Pg Bool
isDraftByAuthor gDraftId gAuthorId = do
  mDraft <- runSelectReturningOne $ select $
              filter_ (\d -> draftId d ==. val_ gDraftId
                             &&. draftAuthorId d ==. val_ gAuthorId)
                      (all_ (dbDraft newsDb))
  pure (isJust mDraft)


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
