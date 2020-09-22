{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

module Queries.Category where


import Control.Monad.Trans.Except (runExceptT)
import Data.Function ((&))
import qualified Data.HashMap.Strict as H
import Data.Int (Int32)
import Data.Text (Text)
import Data.Vector (Vector)

import Data.Aeson
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import BeamSchema
import MaybeOrUnspecified
import Queries.Util


data CreateCategory = CreateCategory
  { cCategoryParentId :: Maybe Int32
  , cCategoryName :: Text
  }
  deriving (Generic, Show)

instance FromJSON CreateCategory where
  parseJSON = genericParseJSON defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop (length ("cCategory" :: String)) }

createCategory :: CreateCategory -> Pg Int32
createCategory cc = do
  [category] <- runInsertReturningList $ insert (dbCategory newsDb) $
    insertExpressions
      [ Category { categoryId       = default_
                 , categoryParentId = val_ (cCategoryParentId cc)
                 , categoryName     = val_ (cCategoryName cc)
                 }
      ]
  pure (categoryId category)


data UpdateCategory = UpdateCategory
  { uCategoryId :: Int32
  , uCategoryNewParentId :: MaybeOrUnspecified Int32
  , uCategoryNewName :: Maybe Text
  }
  deriving Show

instance FromJSON UpdateCategory where
  parseJSON = withObject "UpdateCategory" $ \v -> do
                uCategoryId <- v .: "id"
                uCategoryNewParentId <- case H.lookup "new_parent_id" v of
                  Nothing -> pure Unspecified
                  Just a  -> Specified <$> parseJSON a
                uCategoryNewName <- v .:? "new_name"
                return UpdateCategory {..}

updateCategory :: UpdateCategory -> Pg (Either String ())
updateCategory uc = runExceptT $ do
  makeSureEntityExists "Category" (dbCategory newsDb) categoryId (uCategoryId uc)
  runUpdate $ update (dbCategory newsDb)
                     (\c ->
                          maybeAssignment (uCategoryNewName     uc) (\x -> categoryName     c <-. val_ x)
                       <> case uCategoryNewParentId uc of
                            Unspecified -> mempty
                            Specified newParentId -> categoryParentId c <-. val_ newParentId)
                     (\c -> categoryId c ==. val_ (uCategoryId uc))


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
