module Queries.User where


import Control.Monad.Trans.Except (runExceptT)
import Data.Int (Int32)
import Data.Text (Text)

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

updateUser :: UpdateUser -> Pg (Either String ())
updateUser uu = runExceptT $ do
  makeSureEntityExists "User" (dbUser newsDb) userId (uUserId uu)
  case uUserNewAvatarId uu of
    Nothing ->
      pure ()
    Just newAvatarId ->
      makeSureEntityExists "Photo" (dbPhoto newsDb) photoId newAvatarId
  runUpdate $ update (dbUser newsDb)
                     (\u ->
                          maybeAssignment (uUserNewFirstName uu) (\x -> userFirstName u <-. val_ x)
                       <> maybeAssignment (uUserNewLastName  uu) (\x -> userLastName  u <-. val_ x)
                       <> maybeAssignment (uUserNewAvatarId  uu) (\x -> userAvatarId  u <-. val_ x)
                       <> maybeAssignment (uUserNewIsAdmin   uu) (\x -> userIsAdmin   u <-. val_ x))
                     (\u -> userId u ==. val_ (uUserId uu))
