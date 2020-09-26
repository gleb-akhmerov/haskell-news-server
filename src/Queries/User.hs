module Queries.User where


import Control.Monad.Trans.Except (runExceptT)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (LocalTime)

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
  { uUserNewFirstName :: Maybe Text
  , uUserNewLastName :: Maybe Text
  , uUserNewAvatarId :: Maybe Int32
  , uUserNewIsAdmin :: Maybe Bool
  }
  deriving (Generic, Show)

instance FromJSON UpdateUser where
  parseJSON = genericParseJSON defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop (length "uUser") }

updateUser :: Int32 -> UpdateUser -> Pg (Either String ())
updateUser uUserId uu = runExceptT $ do
  makeSureEntityExists "User" (dbUser newsDb) userId uUserId
  maybeDo (makeSureEntityExists "Photo" (dbPhoto newsDb) photoId) (uUserNewAvatarId uu)
  runUpdate $ update (dbUser newsDb)
                     (\u ->
                          maybeAssignment (uUserNewFirstName uu) (\x -> userFirstName u <-. val_ x)
                       <> maybeAssignment (uUserNewLastName  uu) (\x -> userLastName  u <-. val_ x)
                       <> maybeAssignment (uUserNewAvatarId  uu) (\x -> userAvatarId  u <-. val_ x)
                       <> maybeAssignment (uUserNewIsAdmin   uu) (\x -> userIsAdmin   u <-. val_ x))
                     (\u -> userId u ==. val_ uUserId)


deleteUser :: Int32 -> Pg (Either String ())
deleteUser dUserId = runExceptT $ do
  makeSureEntityExists "User" (dbUser newsDb) userId dUserId
  makeSureNoReferenceExists "User" "Authors" (dbAuthor newsDb) authorId authorId dUserId
  makeSureNoReferenceExists "User" "Commentaries" (dbCommentary newsDb) commentaryUserId commentaryId dUserId
  runDelete $ delete (dbUser newsDb)
    (\c -> userId c ==. val_ dUserId)


data ReturnedUser = ReturnedUser
  { rUserId        :: Int32
  , rUserFirstName :: Text
  , rUserLastName  :: Text
  , rUserAvatarId  :: Int32
  , rUserCreatedAt :: LocalTime
  , rUserIsAdmin   :: Bool
  }
  deriving (Generic, Show)

instance ToJSON ReturnedUser where
  toJSON = genericToJSON defaultOptions
             { fieldLabelModifier = camelTo2 '_' . drop (length "rUser") }

userToReturned :: User -> ReturnedUser
userToReturned a =
  ReturnedUser
    { rUserId        = userId a
    , rUserFirstName = userFirstName a
    , rUserLastName  = userLastName a
    , rUserAvatarId  = userAvatarId a
    , rUserCreatedAt = userCreatedAt a
    , rUserIsAdmin   = userIsAdmin a
    }

getUser :: Int32 -> Pg (Maybe ReturnedUser)
getUser gUserId = do
  mUser <- runSelectReturningOne $ select $
               filter_ (\a -> userId a ==. val_ gUserId)
                       (all_ (dbUser newsDb))
  pure (fmap userToReturned mUser)

getAllUsers :: Pg [ReturnedUser]
getAllUsers = do
  users <- runSelectReturningList $ select $ all_ (dbUser newsDb)
  pure (fmap userToReturned users)
