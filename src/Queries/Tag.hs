module Queries.Tag where


import Control.Monad.Trans.Except (runExceptT)
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
