module Queries.Tag where


import Data.Int (Int32)
import Data.Text (Text)

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import BeamSchema


createTag :: Text -> Pg Int32
createTag tName = do
  [tag] <- runInsertReturningList $ insert (dbTag newsDb) $
    insertExpressions
      [ Tag
          { tagId   = default_
          , tagName = val_ tName
          }
      ]
  pure (tagId tag)
