{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Queries.Post where


import Data.Function ((&))
import Data.Int (Int32)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (LocalTime)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)

import Database.Beam hiding (date)
import Database.Beam.Query.Internal (unsafeRetype)
import Database.Beam.Postgres

import BeamSchema
import Queries.Category
import Queries.Util


type PostsQuery s
  = DbWith
       (DbQ s
         ( PostT (DbQExpr s)
         , UserT (DbQExpr s)
         , CategoryT (DbQExpr s)
         , T2 s (Vector Int32) (Vector Text)
         , T2 s (Vector Int32) (Vector Text)
         , DbQExpr s (Vector Int32)))


postsWithCategories :: PostsQuery s
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
       category <- join_ (dbCategory newsDb) (\c -> postCategoryId post ==. categoryId c)
       pure (post, user, category, cTree, tag, postAdditionalPhoto)
    & aggregate_ (\(post, user, category, cTree, tag, postAdditionalPhoto) ->
                    ( group_ post
                    , group_ user
                    , group_ category
                    , (pgArrayAgg (categoryId cTree), pgArrayAgg (categoryName cTree))
                    , (pgArrayAgg (tagId tag), pgArrayAgg (tagName tag))
                    , pgArrayAgg (postAdditionalPhotoPhotoId postAdditionalPhoto)))


data PostFilter
  = PfPublishedAt LocalTime
  | PfPublishedAtLt LocalTime
  | PfPublishedAtGt LocalTime
  | PfAuthorName Text
  | PfCategoryId Int32
  | PfTagId Int32
  | PfTagIdsIn [Int32]
  | PfTagIdsAll [Int32]
  | PfSearchSubstring Text
  deriving (Eq, Ord)


filterPosts :: Set PostFilter -> PostsQuery s
filterPosts filters =
  foldr applyFilter postsWithCategories filters
  where
    applyFilter flt withQuery = do
      postQuery <- withQuery
      pure $ do
        row@(post, user, category, (_catIds, _catNames), (tagIds, tagNames), _additionalPhotoIds) <- postQuery
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
              val_ (Vector.fromList tIds) `isSubsetOf_` tagIds
            PfTagIdsAll tIds ->
              val_ (Vector.fromList tIds) `isSupersetOf_` tagIds
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
    applyOrdering q = do
      let o = case ord of
                Ascending  -> asc_
                Descending -> desc_
      postQuery <- q
      let ordFunc (post, user, category, _, _, additionalPhotoIds) =
            case by of
              PoPublishedAt  -> o (postPublishedAt post)
              PoAuthorName   -> o (unsafeRetype (concat_ [userFirstName user, " ", userLastName user]))
              PoCategoryName -> o (unsafeRetype (categoryName category))
              PoPhotoCount   -> o (unsafeRetype (arrayLen additionalPhotoIds))
      pure (orderBy_ ordFunc postQuery)
