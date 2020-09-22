{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

module Queries where

import Control.Monad (forM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Int (Int32)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Vector (Vector)

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import BeamSchema

type DbWith = With Postgres NewsDb
type DbQ = Q Postgres NewsDb
type DbQExpr = QExpr Postgres
type T2 s a b = (DbQExpr s a, DbQExpr s b)

withCategoryTree :: DbWith (DbQ s (DbQExpr s Int32, CategoryT (DbQExpr s)))
withCategoryTree = do
  rec catTree <- selecting $
        (do cat <- all_ (dbCategory newsDb)
            pure (categoryId cat, as_ @Int32 1, cat))
        `unionAll_`
        (do (start, ord, cat) <- reuse catTree
            parentCat <- join_ (dbCategory newsDb)
                               (\c -> just_ (categoryId c) ==. categoryParentId cat)
            pure (start, ord + 1, parentCat))
  pure $
    (reuse catTree)
      & orderBy_ (\(start, ord, _c) -> (asc_ start, asc_ ord))
      & fmap (\(start, _ord, c) -> (start, c))

postsWithCategories
  :: DbWith
       (DbQ s
         ( PostT (DbQExpr s)
         , UserT (DbQExpr s)
         , T2 s (Vector Int32) (Vector Text)
         , T2 s (Vector Int32) (Vector Text)
         , DbQExpr s (Vector Int32)))
postsWithCategories = do
  catTree <- withCategoryTree
  pure $
    do post <- all_ (dbPost newsDb)
       (start, cTree) <- catTree
       guard_ (start ==. postCategoryId post)
       postTag <- join_ (dbPostTag newsDb) (\pt -> postTagPostId pt ==. postId post)
       tag <- join_ (dbTag newsDb) (\t -> postTagTagId postTag ==. tagId t)
       postAdditionalPhoto <- join_ (dbPostAdditionalPhoto newsDb) (\pap -> postAdditionalPhotoPostId pap ==. postId post)
       user <- join_ (dbUser newsDb) (\u -> postAuthorId post ==. userId u)
       pure (post, user, cTree, tag, postAdditionalPhoto)
    & aggregate_ (\(post, user, cTree, tag, postAdditionalPhoto) ->
                    ( group_ post
                    , group_ user
                    , (pgArrayAgg (categoryId cTree), pgArrayAgg (categoryName cTree))
                    , (pgArrayAgg (tagId tag), pgArrayAgg (tagName tag))
                    , pgArrayAgg (postAdditionalPhotoPhotoId postAdditionalPhoto)))

categoriesWithTrees :: DbWith (DbQ s (CategoryT (DbQExpr s), DbQExpr s (Vector Int32), DbQExpr s (Vector Text)))
categoriesWithTrees = do
  catTree <- withCategoryTree
  pure $
    do cat <- all_ (dbCategory newsDb)
       (start, cTree) <- catTree
       guard_ (start ==. categoryId cat)
       pure (cat, cTree)
    & aggregate_ (\(cat, cTree) ->
                    ( group_ cat
                    , pgArrayAgg (categoryId cTree)
                    , pgArrayAgg (categoryName cTree)))

data CreateUser = CreateUser
  { cUserFirstName :: Text
  , cUserLastName :: Text
  , cUserAvatar :: ByteString
  , cUserIsAdmin :: Bool
  }

createUser :: CreateUser -> Pg ()
createUser cu =
  runInsert $ insert (dbUser newsDb) $
    insertExpressions
      [ User { userId        = default_
             , userFirstName = val_ (cUserFirstName cu)
             , userLastName  = val_ (cUserLastName cu)
             , userAvatar    = val_ (cUserAvatar cu)
             , userCreatedAt = now_
             , userIsAdmin   = val_ (cUserIsAdmin cu)
             }
      ]

data CreateAuthor = CreateAuthor
  { cAuthorUserId :: Int32
  , cAuthorShortDescription :: Text
  }

createAuthor :: CreateAuthor -> ExceptT String Pg ()
createAuthor ca = do
  do mUser <- runSelectReturningOne $ select $
       filter_ (\a -> userId a ==. val_ (cAuthorUserId ca))
               (all_ (dbUser newsDb))
     when (isNothing mUser) $
       throwE $ "User with id doesn't exist: " ++ show (cAuthorUserId ca)

  runInsert $ insert (dbAuthor newsDb) $
    insertExpressions
      [ Author { authorId               = val_ (cAuthorUserId ca)
               , authorShortDescription = val_ (cAuthorShortDescription ca)
               }
      ]

data CreateCategory = CreateCategory
 { cCategoryParentId :: Maybe Int32
 , cCategoryName :: Text
 }

createCategory :: CreateCategory -> Pg ()
createCategory cc =
  runInsert $ insert (dbCategory newsDb) $
    insertExpressions
      [ Category { categoryId       = default_
                 , categoryParentId = val_ (cCategoryParentId cc)
                 , categoryName     = val_ (cCategoryName cc)
                 }
      ]

data CreateDraft = CreateDraft
  { cDraftShortName :: Text
  , cDraftAuthorId :: Int32
  , cDraftCategoryId :: Int32
  , cDraftTextContent :: Text
  , cDraftMainPhoto :: ByteString
  , cDraftAdditionalPhotos :: [ByteString]
  , cDraftTagIds :: [Int32]
  }

createDraft :: CreateDraft -> ExceptT String Pg ()
createDraft cd = do
  _ <- do mAuthor <- runSelectReturningOne $ select $
            filter_ (\a -> authorId a ==. val_ (cDraftAuthorId cd))
                    (all_ (dbAuthor newsDb))
          when (isNothing mAuthor) $
            throwE $ "Author with id doesn't exist: " ++ show (cDraftAuthorId cd)

          mCategory <- runSelectReturningOne $ select $
            filter_ (\c -> categoryId c ==. val_ (cDraftCategoryId cd))
                    (all_ (dbCategory newsDb))
          when (isNothing mCategory) $
            throwE $ "Category with id doesn't exist: " ++ show (cDraftCategoryId cd)

          forM (cDraftTagIds cd) $ \tId -> do
            mTag <- runSelectReturningOne $ select $
              filter_ (\t -> tagId t ==. val_ tId)
                      (all_ (dbTag newsDb))
            when (isNothing mTag) $
              throwE $ "Tag with id doesn't exist: " ++ show tId

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

createTag :: Text -> Pg ()
createTag tName =
  runInsert $ insert (dbTag newsDb) $
    insertExpressions
      [ Tag
          { tagId   = default_
          , tagName = val_ tName
          }
      ]

data CreateCommentary = CreateCommentary
  { cCommentaryUserId :: Int32
  , cCommentaryPostId :: Int32
  , cCommentaryContent :: Text
  }

createCommentary :: CreateCommentary -> Pg ()
createCommentary ct =
  runInsert $ insert (dbCommentary newsDb) $
    insertExpressions
      [ Commentary
          { commentaryId      = default_
          , commentaryUserId  = val_ (cCommentaryUserId ct)
          , commentaryPostId  = val_ (cCommentaryPostId ct)
          , commentaryContent = val_ (cCommentaryContent ct)
          }
      ]

deleteOrphanedPhotos :: Pg ()
deleteOrphanedPhotos =
  let usedPhotoIds =
        (fmap draftMainPhotoId (all_ (dbDraft newsDb)))
        `union_`
        (fmap postMainPhotoId (all_ (dbPost newsDb)))
        `union_`
        (fmap draftAdditionalPhotoPhotoId (all_ (dbDraftAdditionalPhoto newsDb)))
        `union_`
        (fmap postAdditionalPhotoPhotoId (all_ (dbPostAdditionalPhoto newsDb)))
  in runDelete $ delete (dbPhoto newsDb)
       (\p -> not_ (photoId p `in_` [subquery_ usedPhotoIds]))

publishDraft :: Int32 -> ExceptT String Pg ()
publishDraft dId = do
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

