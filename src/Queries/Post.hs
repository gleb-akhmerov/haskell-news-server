module Queries.Post where


import Data.Function ((&))
import Data.Int (Int32)
import Data.Text (Text)
import Data.Vector (Vector)

import Database.Beam
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
