{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module NewsServer.Database.Author where


import Control.Monad (when)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Function ((&))
import Data.Int (Int32)
import Data.Maybe (isJust)
import Data.Text (Text)

import Data.Aeson
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import NewsServer.Database.BeamSchema
import NewsServer.Database.Util


data CreateAuthor = CreateAuthor
  { cAuthorUserId :: Int32
  , cAuthorShortDescription :: Text
  }
  deriving (Generic, Show)

instance FromJSON CreateAuthor where
  parseJSON = genericParseJSON defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop (length ("cAuthor" :: String)) }

createAuthor :: CreateAuthor -> Pg (Either Text Int32)
createAuthor ca = runExceptT $ do
--  makeSureEntityExists "User" (dbUser newsDb) userId (cAuthorUserId ca)
  mEntity <- runSelectReturningOne $ select $
    join_ (dbAuthor newsDb) (\a -> authorId a ==. val_ (cAuthorUserId ca))
  when (isJust mEntity) $
    throwE $ "User with id is already an author: " <> tshow (cAuthorUserId ca)
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
                { fieldLabelModifier = camelTo2 '_' . drop (length ("uAuthor" :: String)) }

updateAuthor :: Int32 -> UpdateAuthor -> Pg (Either Text ())
updateAuthor uAuthorId ua = runExceptT $ do
  makeSureEntityExists "Author" (dbAuthor newsDb) authorId uAuthorId
  runUpdate $ update (dbAuthor newsDb)
                     (\a -> authorShortDescription a <-. val_ (uAuthorNewShortDescription ua))
                     (\a -> authorId a ==. val_ uAuthorId)

deleteAuthor :: Int32 -> Pg (Either Text ())
deleteAuthor dAuthorId = runExceptT $ do
  makeSureEntityExists "Author" (dbAuthor newsDb) authorId dAuthorId
  makeSureNoReferenceExists "Author" "Drafts" (dbDraft newsDb) draftAuthorId draftId dAuthorId
  makeSureNoReferenceExists "Author"  "Posts" (dbPost  newsDb)  postAuthorId postId dAuthorId
  runDelete $ delete (dbAuthor newsDb)
    (\a -> authorId a ==. val_ dAuthorId)


data ReturnedAuthor = ReturnedAuthor
  { rAuthorId :: Int32
  , rAuthorShortDescription :: Text
  }
  deriving (Generic, Show, Eq)

instance ToJSON ReturnedAuthor where
  toJSON = genericToJSON defaultOptions
             { fieldLabelModifier = camelTo2 '_' . drop (length ("rAuthor" :: String)) }

authorToReturned :: Author -> ReturnedAuthor
authorToReturned a =
  ReturnedAuthor
    { rAuthorId               = authorId a
    , rAuthorShortDescription = authorShortDescription a
    }

getAuthor :: Int32 -> Pg (Maybe ReturnedAuthor)
getAuthor gAuthorId = do
  mAuthor <- runSelectReturningOne $ select $
               filter_ (\a -> authorId a ==. val_ gAuthorId)
                       (all_ (dbAuthor newsDb))
  pure (fmap authorToReturned mAuthor)

getAllAuthors :: Integer -> Pg [ReturnedAuthor]
getAllAuthors pageNum = do
  authors <- runSelectReturningList $ select $
               all_ (dbAuthor newsDb)
               & orderBy_ (asc_ . authorId)
               & offset_ (20 * (pageNum - 1))
               & limit_ 20
  pure (fmap authorToReturned authors)
