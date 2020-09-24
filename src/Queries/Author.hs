module Queries.Author where


import Control.Monad.Trans.Except (runExceptT)
import Data.Int (Int32)
import Data.Text (Text)

import Data.Aeson
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import BeamSchema
import Queries.Util


data CreateAuthor = CreateAuthor
  { cAuthorUserId :: Int32
  , cAuthorShortDescription :: Text
  }
  deriving (Generic, Show)

instance FromJSON CreateAuthor where
  parseJSON = genericParseJSON defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop (length "cAuthor") }

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
  { uAuthorNewShortDescription :: Text
  }
  deriving (Generic, Show)

instance FromJSON UpdateAuthor where
  parseJSON = genericParseJSON defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop (length "uAuthor") }

updateAuthor :: Int32 -> UpdateAuthor -> Pg (Either String ())
updateAuthor uAuthorId ua = runExceptT $ do
  makeSureEntityExists "Author" (dbAuthor newsDb) authorId uAuthorId
  runUpdate $ update (dbAuthor newsDb)
                     (\a -> authorShortDescription a <-. val_ (uAuthorNewShortDescription ua))
                     (\a -> authorId a ==. val_ uAuthorId)

deleteAuthor :: Int32 -> Pg (Either String ())
deleteAuthor dAuthorId = runExceptT $ do
  makeSureEntityExists "Author" (dbAuthor newsDb) authorId dAuthorId
  makeSureNoReferenceExists "Author" "Drafts" (dbDraft newsDb) draftAuthorId draftId dAuthorId
  makeSureNoReferenceExists "Author"  "Posts" (dbPost  newsDb)  postAuthorId postId dAuthorId
  runDelete $ delete (dbAuthor newsDb)
    (\a -> authorId a ==. val_ dAuthorId)
