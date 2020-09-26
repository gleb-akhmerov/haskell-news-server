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
import Database.Beam.Postgres

import BeamSchema
import Queries.Category
import Queries.Util


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


data PostFilter
  = PfPublishedAt LocalTime
  | PfPublishedAtLt LocalTime
  | PfPublishedAtGt LocalTime
  | PfAuthorName Text Text
  | PfCategoryId Int32
  | PfTagId Int32
  | PfTagIdsIn [Int32]
  | PfTagIdsAll [Int32]
  | PfNameSubstring Text
  | PfContentSubstring Text
  deriving (Eq, Ord)


applyFiltersToQuery
  :: Set PostFilter
  -> DbWith
       (DbQ s
         ( PostT (DbQExpr s)
         , UserT (DbQExpr s)
         , T2 s (Vector Int32) (Vector Text)
         , T2 s (Vector Int32) (Vector Text)
         , DbQExpr s (Vector Int32)))
applyFiltersToQuery filters =
  foldr applyFilter postsWithCategories filters
  where
    applyFilter flt withQuery = do
      postQuery <- withQuery
      pure $ do
        row@(post, user, (_catIds, _catNames), (tagIds, _tagNames), _additionalPhotoIds) <- postQuery
        guard_ $
          case flt of
            PfPublishedAt date ->
              postPublishedAt post ==. val_ date
            PfPublishedAtLt date ->
              postPublishedAt post <. val_ date
            PfPublishedAtGt date ->
              postPublishedAt post >. val_ date
            PfAuthorName firstName lastName ->
              userFirstName user ==. val_ firstName &&. userLastName user ==. val_ lastName
            PfCategoryId cId ->
              postCategoryId post ==. val_ cId
            PfTagId tId ->
              val_ (Vector.fromList [tId]) `isSubsetOf_` tagIds
            PfTagIdsIn tIds ->
              val_ (Vector.fromList tIds) `isSubsetOf_` tagIds
            PfTagIdsAll tIds ->
              val_ (Vector.fromList tIds) `isSupersetOf_` tagIds
            _ ->
              val_ True
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
