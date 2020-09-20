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
import Data.Int (Int32)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Vector (Vector)

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import BeamSchema

categoryTree :: With Postgres NewsDb (Q Postgres NewsDb s (QExpr Postgres s Int32, CategoryT (QExpr Postgres s)))
categoryTree = do
  rec catTree <- selecting $
        (do cat <- all_ (_dbCategory newsDb)
            pure (_categoryId cat, as_ @Int32 1, cat))
        `unionAll_`
        (do (start, ord, cat) <- reuse catTree
            parentCat <- join_ (_dbCategory newsDb)
                               (\c -> just_ (_categoryId c) ==. unCategoryId (_categoryParentId cat))
            pure (start, ord + 1, parentCat))
  pure $
    fmap (\(start, _ord, c) -> (start, c)) $
      orderBy_
        (\(start, ord, _c) -> (asc_ start, asc_ ord))
        (reuse catTree)

data Tuple3 a b c = Tuple3 (a, b, c)

postsWithCategories :: With Postgres NewsDb (Q Postgres NewsDb s (PostT (QExpr Postgres s), QExpr Postgres s (Vector Int32), QExpr Postgres s (Vector Text)))
postsWithCategories = do
  cats <- categoryTree
  pure $ aggregate_ (\(post, (start, cat)) ->
                       ( group_ post
                       , pgArrayAgg start
                       , pgArrayAgg (_categoryName cat))) $ do
    post <- all_ (_dbPost newsDb)
    c@(start, _) <- cats
    guard_ (CategoryId start ==. _postCategoryId post)
    pure (post, c)

categoriesWithTrees :: With Postgres NewsDb (Q Postgres NewsDb s (CategoryT (QExpr Postgres s), QExpr Postgres s (Vector Int32), QExpr Postgres s (Vector (Maybe Int32)), QExpr Postgres s (Vector Text)))
categoriesWithTrees = do
  cats <- categoryTree
  pure $ aggregate_ (\(cat, (_start, treeCat)) ->
                       ( group_ cat
                       , pgArrayAgg (_categoryId treeCat)
                       , pgArrayAgg (unCategoryId (_categoryParentId treeCat))
                       , pgArrayAgg (_categoryName treeCat))) $ do
    cat <- all_ (_dbCategory newsDb)
    c@(start, _) <- cats
    guard_ (start ==. _categoryId cat)
    pure (cat, c)

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

