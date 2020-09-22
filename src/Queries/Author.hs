module Queries.Author where


import Control.Monad (when)
import Control.Monad.Trans.Except (throwE, runExceptT)
import Data.Int (Int32)
import Data.Maybe (isNothing)
import Data.Text (Text)

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import BeamSchema


data CreateAuthor = CreateAuthor
  { cAuthorUserId :: Int32
  , cAuthorShortDescription :: Text
  }

createAuthor :: CreateAuthor -> Pg (Either String Int32)
createAuthor ca = runExceptT $ do
  do mUser <- runSelectReturningOne $ select $
       filter_ (\a -> userId a ==. val_ (cAuthorUserId ca))
               (all_ (dbUser newsDb))
     when (isNothing mUser) $
       throwE $ "User with id doesn't exist: " ++ show (cAuthorUserId ca)

  [author] <- runInsertReturningList $ insert (dbAuthor newsDb) $
    insertExpressions
      [ Author { authorId               = val_ (cAuthorUserId ca)
               , authorShortDescription = val_ (cAuthorShortDescription ca)
               }
      ]

  pure (authorId author)
