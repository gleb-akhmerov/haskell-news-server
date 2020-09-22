module Queries.Tag where


import Data.Text (Text)

import Database.Beam
import Database.Beam.Postgres

import BeamSchema


createTag :: Text -> Pg ()
createTag tName =
  runInsert $ insert (dbTag newsDb) $
    insertExpressions
      [ Tag
          { tagId   = default_
          , tagName = val_ tName
          }
      ]

