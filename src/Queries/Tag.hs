module Queries.Tag where


import Control.Monad.Trans.Except (runExceptT)
import Data.Function ((&))
import Data.Int (Int32)
import Data.Text (Text)

import Data.Aeson
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import BeamSchema
import Queries.Util


data CreateTag = CreateTag
  { cTagName :: Text
  }
  deriving (Generic, Show)

instance FromJSON CreateTag where
  parseJSON = genericParseJSON defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop (length "cTag") }

createTag :: CreateTag -> Pg Int32
createTag ct = do
  [tag] <- runInsertReturningList $ insert (dbTag newsDb) $
    insertExpressions
      [ Tag
          { tagId   = default_
          , tagName = val_ (cTagName ct)
          }
      ]
  pure (tagId tag)


data UpdateTag = UpdateTag
  { uTagNewName :: Text
  }
  deriving (Generic, Show)

instance FromJSON UpdateTag where
  parseJSON = genericParseJSON defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop (length "uTag") }

updateTag :: Int32 -> UpdateTag -> Pg (Either String ())
updateTag uTagId ut = runExceptT $ do
  makeSureEntityExists "Tag" (dbTag newsDb) tagId uTagId
  runUpdate $ update (dbTag newsDb)
                     (\t -> tagName t <-. val_ (uTagNewName ut))
                     (\t -> tagId t ==. val_ uTagId)


deleteTag :: Int32 -> Pg (Either String ())
deleteTag dTagId = runExceptT $ do
  makeSureEntityExists "Tag" (dbTag newsDb) tagId dTagId
  makeSureNoReferenceExistsMtm "Tag" "Posts"
                               (dbPostTag newsDb) postTagTagId postTagPostId
                               (dbPost newsDb) postId postId
                               dTagId
  makeSureNoReferenceExistsMtm "Tag" "Drafts"
                               (dbDraftTag newsDb) draftTagTagId draftTagDraftId
                               (dbDraft newsDb) draftId draftId
                               dTagId
  runDelete $
    delete (dbTag newsDb)
      (\t -> tagId t ==. val_ dTagId)


data ReturnedTag = ReturnedTag
  { rTagId :: Int32
  , rTagName :: Text
  }
  deriving (Generic, Show)

instance ToJSON ReturnedTag where
  toJSON = genericToJSON defaultOptions
             { fieldLabelModifier = camelTo2 '_' . drop (length "rTag") }

tagToReturned :: Tag -> ReturnedTag
tagToReturned t =
  ReturnedTag
    { rTagId   = tagId t
    , rTagName = tagName t
    }

getTag :: Int32 -> Pg (Maybe ReturnedTag)
getTag gTagId = do
  mTag <- runSelectReturningOne $ select $
               filter_ (\a -> tagId a ==. val_ gTagId)
                       (all_ (dbTag newsDb))
  pure (fmap tagToReturned mTag)

getAllTags :: Integer -> Pg [ReturnedTag]
getAllTags pageNum = do
  tags <- runSelectReturningList $ select $
            all_ (dbTag newsDb)
            & offset_ (20 * (pageNum - 1))
            & limit_ 20
  pure (fmap tagToReturned tags)
