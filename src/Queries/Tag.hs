module Queries.Tag where


import Control.Monad.Trans.Except (runExceptT)
import Data.Int (Int32)
import Data.Text (Text)

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import BeamSchema
import Queries.Util


data CreateTag = CreateTag
  { cTagName :: Text
  }

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
  { uTagId :: Int32
  , uTagNewName :: Text
  }

updateTag :: UpdateTag -> Pg (Either String ())
updateTag ut = runExceptT $ do
  makeSureEntityExists "Tag" (dbTag newsDb) tagId (uTagId ut)
  runUpdate $ update (dbTag newsDb)
                     (\t -> tagName t <-. val_ (uTagNewName ut))
                     (\t -> tagId t ==. val_ (uTagId ut))
