{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

module Queries.Category where


import Data.Function ((&))
import Data.Int (Int32)
import Data.Text (Text)
import Data.Vector (Vector)

import Database.Beam
import Database.Beam.Postgres

import BeamSchema
import Queries.Util


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
