module Queries.User where


import Control.Monad (when)
import Control.Monad.Trans.Except (throwE, runExceptT)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Maybe (isNothing)
import Data.Text (Text)

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import BeamSchema
import Queries.Util


data CreateUser = CreateUser
  { cUserFirstName :: Text
  , cUserLastName :: Text
  , cUserAvatar :: ByteString
  , cUserIsAdmin :: Bool
  }

createUser :: CreateUser -> Pg Int32
createUser cu = do
  [user] <- runInsertReturningList $ insert (dbUser newsDb) $
    insertExpressions
      [ User { userId        = default_
             , userFirstName = val_ (cUserFirstName cu)
             , userLastName  = val_ (cUserLastName cu)
             , userAvatar    = val_ (cUserAvatar cu)
             , userCreatedAt = now_
             , userIsAdmin   = val_ (cUserIsAdmin cu)
             }
      ]
  pure (userId user)


data UpdateUser = UpdateUser
  { uUserId :: Int32
  , uUserNewFirstName :: Maybe Text
  , uUserNewLastName :: Maybe Text
  , uUserNewAvatar :: Maybe ByteString
  , uUserNewIsAdmin :: Maybe Bool
  }

updateUser :: UpdateUser -> Pg (Either String ())
updateUser uu = runExceptT $ do
  do mUser <- runSelectReturningOne $ select $
       filter_ (\u -> userId u ==. val_ (uUserId uu))
               (all_ (dbUser newsDb))
     when (isNothing mUser) $
       throwE $ "User with id doesn't exist: " ++ show (uUserId uu)

  runUpdate $ update (dbUser newsDb)
                     (\u ->
                          maybeAssignment (uUserNewFirstName uu) (\x -> userFirstName u <-. val_ x)
                       <> maybeAssignment (uUserNewLastName  uu) (\x -> userLastName  u <-. val_ x)
                       <> maybeAssignment (uUserNewAvatar    uu) (\x -> userAvatar    u <-. val_ x)
                       <> maybeAssignment (uUserNewIsAdmin   uu) (\x -> userIsAdmin   u <-. val_ x))
                     (\u -> userId u ==. val_ (uUserId uu))
