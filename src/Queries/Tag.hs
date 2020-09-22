module Queries.Tag where


import Control.Monad (when)
import Control.Monad.Trans.Except (throwE, runExceptT)
import Data.Int (Int32)
import Data.Maybe (isNothing)
import Data.Text (Text)

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import BeamSchema


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
  do mTag <- runSelectReturningOne $ select $
       filter_ (\t -> tagId t ==. val_ (uTagId ut))
               (all_ (dbTag newsDb))
     when (isNothing mTag) $
       throwE $ "Tag with id doesn't exist: " ++ show (uTagId ut)

  runUpdate $ update (dbTag newsDb)
                     (\t -> tagName t <-. val_ (uTagNewName ut))
                     (\t -> tagId t ==. val_ (uTagId ut))
