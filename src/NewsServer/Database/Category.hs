{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module NewsServer.Database.Category where


import Control.Monad.Trans.Except (runExceptT)
import Data.Function ((&))
import qualified Data.HashMap.Strict as H
import Data.Int (Int32)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (zip)

import Data.Aeson
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import NewsServer.Database.BeamSchema
import NewsServer.MaybeOrUnspecified
import NewsServer.Database.Util


data CreateCategory = CreateCategory
  { cCategoryParentId :: Maybe Int32
  , cCategoryName :: Text
  }
  deriving (Generic, Show)

instance FromJSON CreateCategory where
  parseJSON = genericParseJSON defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop (length ("cCategory" :: String)) }

createCategory :: CreateCategory -> Pg (Either Text Int32)
createCategory cc = runExceptT $ do
  maybeDo (makeSureEntityExists "Category" (dbCategory newsDb) categoryId) (cCategoryParentId cc)
  [category] <- runInsertReturningList $ insert (dbCategory newsDb) $
    insertExpressions
      [ Category { categoryId       = default_
                 , categoryParentId = val_ (cCategoryParentId cc)
                 , categoryName     = val_ (cCategoryName cc)
                 }
      ]
  pure (categoryId category)


data UpdateCategory = UpdateCategory
  { uCategoryNewParentId :: MaybeOrUnspecified Int32
  , uCategoryNewName :: Maybe Text
  }
  deriving Show

instance FromJSON UpdateCategory where
  parseJSON = withObject "UpdateCategory" $ \v -> do
                uCategoryNewParentId <- case H.lookup "new_parent_id" v of
                  Nothing -> pure Unspecified
                  Just a  -> Specified <$> parseJSON a
                uCategoryNewName <- v .:? "new_name"
                return UpdateCategory {..}

updateCategory :: Int32 -> UpdateCategory -> Pg (Either Text ())
updateCategory uCategoryId uc = runExceptT $ do
  makeSureEntityExists "Category" (dbCategory newsDb) categoryId uCategoryId
  case uCategoryNewParentId uc of
    Unspecified -> pure ()
    Specified newParentId ->
      maybeDo (makeSureEntityExists "Category" (dbCategory newsDb) categoryId) newParentId
  runUpdate $ update (dbCategory newsDb)
                     (\c ->
                          maybeAssignment (uCategoryNewName     uc) (\x -> categoryName     c <-. val_ x)
                       <> case uCategoryNewParentId uc of
                            Unspecified -> mempty
                            Specified newParentId -> categoryParentId c <-. val_ newParentId)
                     (\c -> categoryId c ==. val_ uCategoryId)


deleteCategory :: Int32 -> Pg (Either Text ())
deleteCategory dCategoryId = runExceptT $ do
  makeSureEntityExists "Category" (dbCategory newsDb) categoryId dCategoryId
  makeSureNoMaybeReferenceExists "Category" "Categories" (dbCategory newsDb) categoryParentId categoryId dCategoryId
  makeSureNoReferenceExists "Category" "Drafts" (dbDraft newsDb) draftCategoryId draftId dCategoryId
  makeSureNoReferenceExists "Category"  "Posts" (dbPost  newsDb)  postCategoryId  postId dCategoryId
  runDelete $ delete (dbCategory newsDb)
    (\c -> categoryId c ==. val_ dCategoryId)


data ReturnedCategory = ReturnedCategory
  { rCategoryId :: Int32
  , rCategoryParent :: Maybe ReturnedCategory
  , rCategoryName :: Text
  }
  deriving (Generic, Show, Eq)

instance ToJSON ReturnedCategory where
  toJSON = genericToJSON defaultOptions
             { fieldLabelModifier = camelTo2 '_' . drop (length ("rCategory" :: String)) }

categoriesToReturned :: Foldable f => f Category -> Maybe ReturnedCategory
categoriesToReturned cats =
  foldr f Nothing cats
  where f c parent =
          Just $
            ReturnedCategory
              { rCategoryId     = categoryId c
              , rCategoryParent = parent
              , rCategoryName   = categoryName c
              }

getCategory :: Int32 -> Pg (Maybe ReturnedCategory)
getCategory gCategoryId = do
  cats <- runSelectReturningList $ selectWith $ do
            catTreeQuery <- withCategoryTree
            pure $ do
              (start, cat) <- catTreeQuery
              guard_ (start ==. val_ gCategoryId)
              pure cat
  pure $ categoriesToReturned cats

categoryTupleToReturned :: (Vector Int32, Vector Text) -> ReturnedCategory
categoryTupleToReturned (ids, names) =
  fromJust $ foldr convert Nothing (Vector.zip ids names)
  where
    convert (id_, name) parent =
      Just $
        ReturnedCategory
          { rCategoryId = id_
          , rCategoryParent = parent
          , rCategoryName = name
          }

getAllCategories :: Integer -> Pg [ReturnedCategory]
getAllCategories pageNum = do
  rows <- runSelectReturningList $ selectWith $ do
            groupedCategoryIdsNamesQuery <- groupedCategoryIdsNames
            pure $
              groupedCategoryIdsNamesQuery
              & offset_ (20 * (pageNum - 1))
              & limit_ 20
  pure $ fmap categoryTupleToReturned rows

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


groupedCategoryIdsNames :: DbWith (DbQ s (DbQExpr s (Vector Int32), DbQExpr s (Vector Text)))
groupedCategoryIdsNames = do
  catTreeQuery <- withCategoryTree
  pure $
    do cat <- all_ (dbCategory newsDb)
       (start, cTree) <- catTreeQuery
       guard_ (start ==. categoryId cat)
       pure (cat, cTree)
    & aggregate_ (\(cat, cTree) ->
                    ( group_ cat
                    , pgArrayAgg (categoryId cTree)
                    , pgArrayAgg (categoryName cTree)))
    & fmap (\(_, ids, names) -> (ids, names))
