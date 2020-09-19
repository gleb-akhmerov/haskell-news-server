{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Queries where

import Control.Monad (forM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Maybe (isNothing)
import Data.String (IsString)
import Data.Text (Text)
import Data.Vector (Vector)

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import BeamSchema

categoryWithParentsImpl :: (Monoid a, IsString a) => a -> a
categoryWithParentsImpl categoryId =
  "(with recursive category_tree as (\
  \  select c1.name, c1.parent_id from category c1\
  \  where id = " <> categoryId <>
  "    union all\
  \  select c2.name, c2.parent_id from category c2\
  \  join category_tree on category_tree.parent_id = c2.id\
  \)\
  \select array_agg(name) from category_tree)"

categoryWithParents :: C (QExpr Postgres s) Int32 -> Q Postgres NewsDb s (QGenExpr e Postgres s (Vector Text))
categoryWithParents =
  let expr = customExpr_ categoryWithParentsImpl :: C (QExpr Postgres s) Int32 -> QGenExpr e Postgres s (Vector Text)
  in pure . expr

postsWithCategories :: Q Postgres NewsDb s (PostT (QExpr Postgres s), QGenExpr e Postgres s (Vector Text))
postsWithCategories = do
  post <- all_ (_dbPost newsDb)
  cats <- categoryWithParents (unCategoryId (_postCategoryId post))
  pure (post, cats)

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
      [ Author { _authorId               = val_ (UsrId (cAuthorUserId ca))
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
                 , _categoryParentId = val_ (CategoryId (cCategoryParentId cc))
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
            filter_ (\a -> _authorId a ==. val_ (UsrId (cDraftAuthorId cd)))
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
              throwE $ "Tag with id doesn't exist" ++ show tId

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
          , _draftAuthorId    = val_ (AuthorId (UsrId (cDraftAuthorId cd)))
          , _draftCategoryId  = val_ (CategoryId (cDraftCategoryId cd))
          , _draftTextContent = val_ (cDraftTextContent cd)
          , _draftMainPhotoId = val_ (pk mainPhoto)
          }
      ]

  let additionalPhotoToRow p =
        DraftAdditionalPhoto
          { _draftAdditionalPhotoPhotoId = pk p
          , _draftAdditionalPhotoDraftId = pk draft
          }
  runInsert $ insert (_dbDraftAdditionalPhoto newsDb) $
    insertValues (map additionalPhotoToRow additionalPhotos)

  let tagToRow tId =
        DraftTag
          { _draftTagTagId   = TagId tId
          , _draftTagDraftId = pk draft
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
          , _commentaryUserId  = val_ (UsrId (cCommentaryUserId ct))
          , _commentaryPostId  = val_ (PostId (DraftId (cCommentaryPostId ct)))
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
       (\p -> not_ (_photoId p `in_` [subquery_ $ fmap unPhotoId usedPhotoIds]))

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
              { _postId          = val_ (pk draft)
              , _postShortName   = val_ (_draftShortName draft)
              , _postPublishedAt = now_
              , _postAuthorId    = val_ (_draftAuthorId draft)
              , _postCategoryId  = val_ (_draftCategoryId draft)
              , _postTextContent = val_ (_draftTextContent draft)
              , _postMainPhotoId = val_ (_draftMainPhotoId draft)
              }
      [insertedPost] <- runInsertReturningList $ insertOnConflict (_dbPost newsDb)
        (insertExpressions [newPost])
        (conflictingFields pk)
        (onConflictUpdateSet (\fields _oldValues -> fields <-. newPost))

      runDelete $ delete (_dbPostTag newsDb)
        (\pt -> _postTagPostId pt ==. val_ (pk insertedPost))
      draftTags <- runSelectReturningList $ select $
        oneToMany_ (_dbDraftTag newsDb) _draftTagDraftId (val_ draft)
      let draftTagToRow draftTag =
            PostTag
              { _postTagTagId  = _draftTagTagId draftTag
              , _postTagPostId = PostId (pk draft)
              }
      runInsert $ insert (_dbPostTag newsDb) $
        insertValues (map draftTagToRow draftTags)

      runDelete $ delete (_dbPostAdditionalPhoto newsDb)
        (\pap -> _postAdditionalPhotoPostId pap ==. val_ (pk insertedPost))
      draftPhotos <- runSelectReturningList $ select $
        oneToMany_ (_dbDraftAdditionalPhoto newsDb) _draftAdditionalPhotoDraftId (val_ draft)
      let draftAdditionalPhotoToRow dap =
            PostAdditionalPhoto
              { _postAdditionalPhotoPhotoId = _draftAdditionalPhotoPhotoId dap
              , _postAdditionalPhotoPostId  = PostId (pk draft)
              }
      runInsert $ insert (_dbPostAdditionalPhoto newsDb) $
        insertValues (map draftAdditionalPhotoToRow draftPhotos)

      lift $ deleteOrphanedPhotos

