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
        (do cat <- all_ (_dbCategory newsDb)
            pure (_categoryId cat, as_ @Int32 1, cat))
        `unionAll_`
        (do (start, ord, cat) <- reuse catTree
            parentCat <- join_ (_dbCategory newsDb)
                               (\c -> just_ (_categoryId c) ==. _categoryParentId cat)
            pure (start, ord + 1, parentCat))
  pure $
    (reuse catTree)
      & orderBy_ (\(start, ord, _c) -> (asc_ start, asc_ ord))
      & fmap (\(start, _ord, c) -> (start, c))

postsWithCategories
  :: DbWith
       (DbQ s
         ( PostT (DbQExpr s)
         , UsrT (DbQExpr s)
         , T2 s (Vector Int32) (Vector Text)
         , T2 s (Vector Int32) (Vector Text)
         , DbQExpr s (Vector Int32)))
postsWithCategories = do
  catTree <- withCategoryTree
  pure $
    do post <- all_ (_dbPost newsDb)
       (start, cTree) <- catTree
       guard_ (start ==. _postCategoryId post)
       postTag <- join_ (_dbPostTag newsDb) (\pt -> _postTagPostId pt ==. _postId post)
       tag <- join_ (_dbTag newsDb) (\t -> _postTagTagId postTag ==. _tagId t)
       postAdditionalPhoto <- join_ (_dbPostAdditionalPhoto newsDb) (\pap -> _postAdditionalPhotoPostId pap ==. _postId post)
       user <- join_ (_dbUsr newsDb) (\u -> _postAuthorId post ==. _usrId u)
       pure (post, user, cTree, tag, postAdditionalPhoto)
    & aggregate_ (\(post, user, cTree, tag, postAdditionalPhoto) ->
                    ( group_ post
                    , group_ user
                    , (pgArrayAgg (_categoryId cTree), pgArrayAgg (_categoryName cTree))
                    , (pgArrayAgg (_tagId tag), pgArrayAgg (_tagName tag))
                    , pgArrayAgg (_postAdditionalPhotoPhotoId postAdditionalPhoto)))

categoriesWithTrees :: DbWith (DbQ s (CategoryT (DbQExpr s), DbQExpr s (Vector Int32), DbQExpr s (Vector Text)))
categoriesWithTrees = do
  catTree <- withCategoryTree
  pure $
    do cat <- all_ (_dbCategory newsDb)
       (start, cTree) <- catTree
       guard_ (start ==. _categoryId cat)
       pure (cat, cTree)
    & aggregate_ (\(cat, cTree) ->
                    ( group_ cat
                    , pgArrayAgg (_categoryId cTree)
                    , pgArrayAgg (_categoryName cTree)))

data CreateUser = CreateUser
  { cUserFirstName :: Text
  , cUserLastName :: Text
  , cUserAvatar :: ByteString
  , cUserIsAdmin :: Bool
  }

createUser :: CreateUser -> Pg ()
createUser cu =
  runInsert $ insert (_dbUsr newsDb) $
    insertExpressions
      [ Usr { _usrId        = default_
            , _usrFirstName = val_ (cUserFirstName cu)
            , _usrLastName  = val_ (cUserLastName cu)
            , _usrAvatar    = val_ (cUserAvatar cu)
            , _usrCreatedAt = now_
            , _usrIsAdmin   = val_ (cUserIsAdmin cu)
            }
      ]

data CreateAuthor = CreateAuthor
  { cAuthorUserId :: Int32
  , cAuthorShortDescription :: Text
  }

createAuthor :: CreateAuthor -> ExceptT String Pg ()
createAuthor ca = do
  do mUser <- runSelectReturningOne $ select $
       filter_ (\a -> _usrId a ==. val_ (cAuthorUserId ca))
               (all_ (_dbUsr newsDb))
     when (isNothing mUser) $
       throwE $ "User with id doesn't exist: " ++ show (cAuthorUserId ca)

  runInsert $ insert (_dbAuthor newsDb) $
    insertExpressions
      [ Author { _authorId               = val_ (cAuthorUserId ca)
               , _authorShortDescription = val_ (cAuthorShortDescription ca)
               }
      ]

data CreateCategory = CreateCategory
 { cCategoryParentId :: Maybe Int32
 , cCategoryName :: Text
 }

createCategory :: CreateCategory -> Pg ()
createCategory cc =
  runInsert $ insert (_dbCategory newsDb) $
    insertExpressions
      [ Category { _categoryId       = default_
                 , _categoryParentId = val_ (cCategoryParentId cc)
                 , _categoryName     = val_ (cCategoryName cc)
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
            filter_ (\a -> _authorId a ==. val_ (cDraftAuthorId cd))
                    (all_ (_dbAuthor newsDb))
          when (isNothing mAuthor) $
            throwE $ "Author with id doesn't exist: " ++ show (cDraftAuthorId cd)

          mCategory <- runSelectReturningOne $ select $
            filter_ (\c -> _categoryId c ==. val_ (cDraftCategoryId cd))
                    (all_ (_dbCategory newsDb))
          when (isNothing mCategory) $
            throwE $ "Category with id doesn't exist: " ++ show (cDraftCategoryId cd)

          forM (cDraftTagIds cd) $ \tId -> do
            mTag <- runSelectReturningOne $ select $
              filter_ (\t -> _tagId t ==. val_ tId)
                      (all_ (_dbTag newsDb))
            when (isNothing mTag) $
              throwE $ "Tag with id doesn't exist: " ++ show tId

  let photoToRow p =
        Photo
          { _photoId      = default_
          , _photoContent = val_ p
          }
  [mainPhoto] <- runInsertReturningList $ insert (_dbPhoto newsDb) $
    insertExpressions [photoToRow (cDraftMainPhoto cd)]
  additionalPhotos <- runInsertReturningList $ insert (_dbPhoto newsDb) $
    insertExpressions (map photoToRow (cDraftAdditionalPhotos cd))

  [draft] <- runInsertReturningList $ insert (_dbDraft newsDb) $
    insertExpressions
      [ Draft
          { _draftId          = default_
          , _draftShortName   = val_ (cDraftShortName cd)
          , _draftCreatedAt   = now_
          , _draftAuthorId    = val_ (cDraftAuthorId cd)
          , _draftCategoryId  = val_ (cDraftCategoryId cd)
          , _draftTextContent = val_ (cDraftTextContent cd)
          , _draftMainPhotoId = val_ (_photoId mainPhoto)
          }
      ]

  let additionalPhotoToRow p =
        DraftAdditionalPhoto
          { _draftAdditionalPhotoPhotoId = _photoId p
          , _draftAdditionalPhotoDraftId = _draftId draft
          }
  runInsert $ insert (_dbDraftAdditionalPhoto newsDb) $
    insertValues (map additionalPhotoToRow additionalPhotos)

  let tagToRow tId =
        DraftTag
          { _draftTagTagId   = tId
          , _draftTagDraftId = _draftId draft
          }
  runInsert $ insert (_dbDraftTag newsDb) $
    insertValues (map tagToRow (cDraftTagIds cd))

createTag :: Text -> Pg ()
createTag tagName =
  runInsert $ insert (_dbTag newsDb) $
    insertExpressions
      [ Tag
          { _tagId   = default_
          , _tagName = val_ tagName
          }
      ]

data CreateCommentary = CreateCommentary
  { cCommentaryUserId :: Int32
  , cCommentaryPostId :: Int32
  , cCommentaryContent :: Text
  }

createCommentary :: CreateCommentary -> Pg ()
createCommentary ct =
  runInsert $ insert (_dbCommentary newsDb) $
    insertExpressions
      [ Commentary
          { _commentaryId      = default_
          , _commentaryUserId  = val_ (cCommentaryUserId ct)
          , _commentaryPostId  = val_ (cCommentaryPostId ct)
          , _commentaryContent = val_ (cCommentaryContent ct)
          }
      ]

deleteOrphanedPhotos :: Pg ()
deleteOrphanedPhotos =
  let usedPhotoIds =
        (fmap _draftMainPhotoId (all_ (_dbDraft newsDb)))
        `union_`
        (fmap _postMainPhotoId (all_ (_dbPost newsDb)))
        `union_`
        (fmap _draftAdditionalPhotoPhotoId (all_ (_dbDraftAdditionalPhoto newsDb)))
        `union_`
        (fmap _postAdditionalPhotoPhotoId (all_ (_dbPostAdditionalPhoto newsDb)))
  in runDelete $ delete (_dbPhoto newsDb)
       (\p -> not_ (_photoId p `in_` [subquery_ usedPhotoIds]))

publishDraft :: Int32 -> ExceptT String Pg ()
publishDraft draftId = do
  mDraft <- runSelectReturningOne $ select $
    filter_ (\d -> _draftId d ==. val_ draftId) (all_ (_dbDraft newsDb))
  case mDraft of
    Nothing ->
      throwE $ "Draft with id doesn't exist: " ++ show draftId
    Just draft -> do
      let newPost :: PostT (QExpr Postgres s)
          newPost =
            Post
              { _postId          = val_ (_draftId draft)
              , _postShortName   = val_ (_draftShortName draft)
              , _postPublishedAt = now_
              , _postAuthorId    = val_ (_draftAuthorId draft)
              , _postCategoryId  = val_ (_draftCategoryId draft)
              , _postTextContent = val_ (_draftTextContent draft)
              , _postMainPhotoId = val_ (_draftMainPhotoId draft)
              }
      [insertedPost] <- runInsertReturningList $ insertOnConflict (_dbPost newsDb)
        (insertExpressions [newPost])
        (conflictingFields _postId)
        (onConflictUpdateSet (\fields _oldValues -> fields <-. newPost))

      runDelete $ delete (_dbPostTag newsDb)
        (\pt -> _postTagPostId pt ==. val_ (_postId insertedPost))
      draftTags <- runSelectReturningList $ select $
        join_ (_dbDraftTag newsDb) (\dt -> _draftTagDraftId dt ==. val_ (_draftId draft))
      let draftTagToRow draftTag =
            PostTag
              { _postTagTagId  = _draftTagTagId draftTag
              , _postTagPostId = _draftId draft
              }
      runInsert $ insert (_dbPostTag newsDb) $
        insertValues (map draftTagToRow draftTags)

      runDelete $ delete (_dbPostAdditionalPhoto newsDb)
        (\pap -> _postAdditionalPhotoPostId pap ==. val_ (_postId insertedPost))
      draftPhotos <- runSelectReturningList $ select $
        join_ (_dbDraftAdditionalPhoto newsDb) (\dap -> _draftAdditionalPhotoDraftId dap ==. (val_ (_draftId draft)))
      let draftAdditionalPhotoToRow dap =
            PostAdditionalPhoto
              { _postAdditionalPhotoPhotoId = _draftAdditionalPhotoPhotoId dap
              , _postAdditionalPhotoPostId  = _draftId draft
              }
      runInsert $ insert (_dbPostAdditionalPhoto newsDb) $
        insertValues (map draftAdditionalPhotoToRow draftPhotos)

      lift $ deleteOrphanedPhotos

