{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Queries.Post where


import Data.Function ((&))
import Data.Int (Int32)
import Data.Maybe (isJust, fromMaybe, fromJust)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (LocalTime)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList, toList, zip)

import Data.Aeson
import Database.Beam hiding (date)
import Database.Beam.Query.Internal (unsafeRetype)
import Database.Beam.Postgres

import BeamSchema
import Queries.Category
import Queries.Tag
import Queries.Util


type PostsQuery s
  = (DbQ s
      ( PostT (DbQExpr s)
      , UserT (DbQExpr s)
      , AuthorT (DbQExpr s)
      , CategoryT (DbQExpr s)
      , T2 s (Vector Int32) (Vector Text)
      , DbQExpr s (Vector Int32)))


postsWithNestedEntities :: PostsQuery s
postsWithNestedEntities = do
  post <- all_ (dbPost newsDb)
  postTag <- leftJoin_ (all_ (dbPostTag newsDb)) (\pt -> postTagPostId pt ==. postId post)
  tag <- leftJoin_ (all_ (dbTag newsDb)) (\t -> postTagTagId postTag ==. just_ (tagId t))
  postAdditionalPhoto <- leftJoin_ (all_ (dbPostAdditionalPhoto newsDb)) (\pap -> postAdditionalPhotoPostId pap ==. postId post)
  user <- join_ (dbUser newsDb) (\u -> postAuthorId post ==. userId u)
  author <- join_ (dbAuthor newsDb) (\a -> postAuthorId post ==. authorId a)
  category <- join_ (dbCategory newsDb) (\c -> postCategoryId post ==. categoryId c)
  pure (post, user, author, category, tag, postAdditionalPhoto)
  & aggregate_ (\(post, user, author, category, tag, postAdditionalPhoto) ->
                  ( group_ post
                  , group_ user
                  , group_ author
                  , group_ category
                  , (removeNullsAgg (pgArrayAgg (tagId tag)), removeNullsAgg (pgArrayAgg (tagName tag)))
                  , removeNullsAgg (pgArrayAgg (postAdditionalPhotoPhotoId postAdditionalPhoto))))


data PostFilter
  = PfPublishedAt LocalTime
  | PfPublishedAtLt LocalTime
  | PfPublishedAtGt LocalTime
  | PfAuthorName Text
  | PfCategoryId Int32
  | PfTagId Int32
  | PfTagIdsIn [Int32]
  | PfTagIdsAll [Int32]
  | PfPostNameSubstring Text
  | PfPostContentSubstring Text
  | PfSearchSubstring Text
  deriving (Eq, Ord)


filterPosts :: Set PostFilter -> PostsQuery s
filterPosts filters =
  foldr applyFilter postsWithNestedEntities filters
  where
    applyFilter flt postQuery = do
      row@(post, user, _author, category, (tagIds, tagNames), _additionalPhotoIds) <- postQuery
      guard_ $
        case flt of
          PfPublishedAt date ->
            postPublishedAt post ==. val_ date
          PfPublishedAtLt date ->
            postPublishedAt post <. val_ date
          PfPublishedAtGt date ->
            postPublishedAt post >. val_ date
          PfAuthorName name ->
            userFirstName user `like_` val_ name ||. userLastName user `like_` val_ name
          PfCategoryId cId ->
            postCategoryId post ==. val_ cId
          PfTagId tId ->
            val_ (Vector.fromList [tId]) `isSubsetOf_` tagIds
          PfTagIdsIn tIds ->
            tagIds `isSubsetOf_` val_ (Vector.fromList tIds) &&. arrayLen tagIds >. val_ 0
          PfTagIdsAll tIds ->
            tagIds `isSupersetOf_` val_ (Vector.fromList tIds)
          PfPostNameSubstring substring ->
            postShortName post `like_` val_ ("%" <> substring <> "%")
          PfPostContentSubstring substring ->
            postTextContent post `like_` val_ ("%" <> substring <> "%")
          PfSearchSubstring substring ->
            let ss = val_ ("%" <> substring <> "%")
            in postTextContent post `like_` ss
               ||. userFirstName user `like_` ss
               ||. userLastName user `like_` ss
               ||. categoryName category `like_` ss
               ||. (subquery_ $
                      do tag <- pgUnnestArray tagNames
                         pure $ tag `like_` ss)
      pure row


data Order
  = Ascending
  | Descending

data PostOrder = PostOrder Order PostOrderBy

data PostOrderBy
  = PoPublishedAt
  | PoAuthorName
  | PoCategoryName
  | PoPhotoCount


filterAndSortPosts :: Set PostFilter -> PostOrder -> PostsQuery s
filterAndSortPosts filters (PostOrder ord by) =
  applyOrdering (filterPosts filters)
  where
    applyOrdering q =
      let o = case ord of
                Ascending  -> asc_
                Descending -> desc_
          ordFunc (post, user, _, category, _, additionalPhotoIds) =
            case by of
              PoPublishedAt  -> o (postPublishedAt post)
              PoAuthorName   -> o (unsafeRetype (concat_ [userFirstName user, " ", userLastName user]))
              PoCategoryName -> o (unsafeRetype (categoryName category))
              PoPhotoCount   -> o (unsafeRetype (arrayLen additionalPhotoIds))
      in orderBy_ ordFunc q


data ReturnedPostAuthor = ReturnedPostAuthor
  { rPostAuthorId               :: Int32
  , rPostAuthorFirstName        :: Text
  , rPostAuthorLastName         :: Text
  , rPostAuthorAvatarId         :: Int32
  , rPostAuthorIsAdmin          :: Bool
  , rPostAuthorShortDescription :: Text
  }
  deriving (Generic, Show, Eq)

instance ToJSON ReturnedPostAuthor where
  toJSON = genericToJSON defaultOptions
             { fieldLabelModifier = camelTo2 '_' . drop (length ("rPostAuthor" :: String)) }

data ReturnedPost = ReturnedPost
  { rPostId                 :: Int32
  , rPostShortName          :: Text
  , rPostPublishedAt        :: LocalTime
  , rPostAuthor             :: ReturnedPostAuthor
  , rPostCategory           :: ReturnedCategory
  , rPostTags               :: [ReturnedTag]
  , rPostTextContent        :: Text
  , rPostMainPhotoId        :: Int32
  , rPostAdditionalPhotoIds :: [Int32]
  }
  deriving (Generic, Show, Eq)

instance ToJSON ReturnedPost where
  toJSON = genericToJSON defaultOptions
             { fieldLabelModifier = camelTo2 '_' . drop (length ("rPost" :: String)) }

tagTuplesToReturned :: (Vector Int32, Vector Text) -> [ReturnedTag]
tagTuplesToReturned (ids, names) =
  Vector.toList $ fmap convert (Vector.zip ids names)
  where
    convert (id_, name) =
      ReturnedTag
        { rTagId   = id_
        , rTagName = name
        }

makePostAuthor :: User -> Author -> ReturnedPostAuthor
makePostAuthor user author =
  ReturnedPostAuthor
    { rPostAuthorId               = userId user
    , rPostAuthorFirstName        = userFirstName user
    , rPostAuthorLastName         = userLastName user
    , rPostAuthorAvatarId         = userAvatarId user
    , rPostAuthorIsAdmin          = userIsAdmin user
    , rPostAuthorShortDescription = authorShortDescription author
    }

postToReturning :: ReturnedCategory -> (Post, User, Author, Category, (Vector Int32, Vector Text), Vector Int32) -> ReturnedPost
postToReturning category (post, user, author, _category, (tagIds, tagNames), additionalPhotoIds) =
  ReturnedPost
    { rPostId = postId post
    , rPostShortName = postShortName post
    , rPostPublishedAt = postPublishedAt post
    , rPostAuthor = makePostAuthor user author
    , rPostCategory = category
    , rPostTags = tagTuplesToReturned (tagIds, tagNames)
    , rPostTextContent = postTextContent post
    , rPostMainPhotoId = postMainPhotoId post
    , rPostAdditionalPhotoIds = Vector.toList additionalPhotoIds
    }

getPost :: Int32 -> Pg (Maybe ReturnedPost)
getPost gPostId = do
  mPost <- runSelectReturningOne $ select $ do
             filter_ (\(p, _, _, _, _, _) -> postId p ==. val_ gPostId)
                     postsWithNestedEntities
  case mPost of
    Nothing ->
      pure Nothing
    Just res@(_, _, _, c, _, _) -> do
      category <- getCategory (categoryId c)
      pure (Just (postToReturning (fromJust category) res))

getPosts :: Set PostFilter -> Maybe PostOrder -> Integer -> Pg [ReturnedPost]
getPosts filters mOrder pageNum = do
  let order = fromMaybe (PostOrder Ascending PoPublishedAt) mOrder
  posts <- runSelectReturningList $ select $
    filterAndSortPosts filters order
    & offset_ (20 * (pageNum - 1))
    & limit_ 20
  let catIds = (fmap (\(_, _, _, c, _, _) -> categoryId c) posts)
  categories <- mapM getCategory catIds
  pure (zipWith postToReturning (fmap fromJust categories) posts)

isCommentByUser :: Int32 -> Int32 -> Pg Bool
isCommentByUser gCommentId gUserId = do
  mComment <- runSelectReturningOne $ select $
                filter_ (\c -> commentaryId c ==. val_ gCommentId
                               &&. commentaryUserId c ==. val_ gUserId)
                        (all_ (dbCommentary newsDb))
  pure (isJust mComment)
