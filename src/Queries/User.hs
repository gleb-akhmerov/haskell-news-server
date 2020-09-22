module Queries.User where


import Control.Monad.Trans.Except (runExceptT)
import Data.Int (Int32)
import Data.Text (Text)

import Data.Aeson
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import BeamSchema
import Queries.Util


data CreateUser = CreateUser
  { cUserFirstName :: Text
  , cUserLastName :: Text
  , cUserAvatarId :: Int32
  , cUserIsAdmin :: Bool
  }
  deriving (Generic, Show)

instance FromJSON CreateUser where
  parseJSON = genericParseJSON defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop (length "cUser") }

createUser :: CreateUser -> Pg (Either String Int32)
createUser cu = runExceptT $ do
  makeSureEntityExists "Photo" (dbPhoto newsDb) photoId (cUserAvatarId cu)
  [user] <- runInsertReturningList $ insert (dbUser newsDb) $
    insertExpressions
      [ User { userId        = default_
             , userFirstName = val_ (cUserFirstName cu)
             , userLastName  = val_ (cUserLastName cu)
             , userAvatarId  = val_ (cUserAvatarId cu)
             , userCreatedAt = now_
             , userIsAdmin   = val_ (cUserIsAdmin cu)
             }
      ]
  pure (userId user)


data UpdateUser = UpdateUser
  { uUserId :: Int32
  , uUserNewFirstName :: Maybe Text
  , uUserNewLastName :: Maybe Text
  , uUserNewAvatarId :: Maybe Int32
  , uUserNewIsAdmin :: Maybe Bool
  }
  deriving (Generic, Show)

instance FromJSON UpdateUser where
  parseJSON = genericParseJSON defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop (length "uUser") }

updateUser :: UpdateUser -> Pg (Either String ())
updateUser uu = runExceptT $ do
  makeSureEntityExists "User" (dbUser newsDb) userId (uUserId uu)
  maybeDo (makeSureEntityExists "Photo" (dbPhoto newsDb) photoId) (uUserNewAvatarId uu)
  runUpdate $ update (dbUser newsDb)
                     (\u ->
                          maybeAssignment (uUserNewFirstName uu) (\x -> userFirstName u <-. val_ x)
                       <> maybeAssignment (uUserNewLastName  uu) (\x -> userLastName  u <-. val_ x)
                       <> maybeAssignment (uUserNewAvatarId  uu) (\x -> userAvatarId  u <-. val_ x)
                       <> maybeAssignment (uUserNewIsAdmin   uu) (\x -> userIsAdmin   u <-. val_ x))
                     (\u -> userId u ==. val_ (uUserId uu))
