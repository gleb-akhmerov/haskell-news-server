{-# LANGUAGE OverloadedStrings #-}

module Queries where

import Data.Int (Int32)
import Data.String (IsString)
import Data.Text (Text)
import Data.Vector (Vector)

import Database.Beam
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
