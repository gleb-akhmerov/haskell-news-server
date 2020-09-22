module Queries.Author where


import Control.Monad.Trans.Except (runExceptT)
import Data.Int (Int32)
import Data.Text (Text)

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import BeamSchema
import Queries.Util


data CreateAuthor = CreateAuthor
  { cAuthorUserId :: Int32
  , cAuthorShortDescription :: Text
  }

createAuthor :: CreateAuthor -> Pg (Either String Int32)
createAuthor ca = runExceptT $ do
  makeSureEntityExists "User" (dbUser newsDb) userId (cAuthorUserId ca)
  [author] <- runInsertReturningList $ insert (dbAuthor newsDb) $
    insertExpressions
      [ Author { authorId               = val_ (cAuthorUserId ca)
               , authorShortDescription = val_ (cAuthorShortDescription ca)
               }
      ]
  pure (authorId author)


data UpdateAuthor = UpdateAuthor
  { uAuthorId :: Int32
  , uAuthorNewShortDescription :: Text
  }

updateAuthor :: UpdateAuthor -> Pg (Either String ())
updateAuthor ua = runExceptT $ do
  makeSureEntityExists "Author" (dbAuthor newsDb) authorId (uAuthorId ua)
  runUpdate $ update (dbAuthor newsDb)
                     (\a -> authorShortDescription a <-. val_ (uAuthorNewShortDescription ua))
                     (\a -> authorId a ==. val_ (uAuthorId ua))
