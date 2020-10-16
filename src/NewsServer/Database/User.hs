module NewsServer.Database.User where


import Control.Monad.Trans.Except (runExceptT)
import Data.Function ((&))
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (LocalTime)

import Data.Aeson
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import NewsServer.Database.BeamSchema
import NewsServer.Database.Util


data CreateUser = CreateUser
  { cUserFirstName :: Text
  , cUserLastName :: Text
  , cUserAvatarId :: Int32
  }
  deriving (Generic, Show)

instance FromJSON CreateUser where
  parseJSON = genericParseJSON defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop (length ("cUser" :: String)) }

createUser :: CreateUser -> Pg (Either Text Int32)
createUser cu = runExceptT $ do
  makeSureEntityExists "Photo" (dbPhoto newsDb) photoId (cUserAvatarId cu)
  [user] <- runInsertReturningList $ insert (dbUser newsDb) $
    insertExpressions
      [ User { userId        = default_
             , userFirstName = val_ (cUserFirstName cu)
             , userLastName  = val_ (cUserLastName cu)
             , userAvatarId  = val_ (cUserAvatarId cu)
             , userCreatedAt = now_
             , userIsAdmin   = val_ False
             }
      ]
  pure (userId user)


createAdminUser :: CreateUser -> Pg (Either Text Int32)
createAdminUser cu = runExceptT $ do
  makeSureEntityExists "Photo" (dbPhoto newsDb) photoId (cUserAvatarId cu)
  [user] <- runInsertReturningList $ insert (dbUser newsDb) $
    insertExpressions
      [ User { userId        = default_
             , userFirstName = val_ (cUserFirstName cu)
             , userLastName  = val_ (cUserLastName cu)
             , userAvatarId  = val_ (cUserAvatarId cu)
             , userCreatedAt = now_
             , userIsAdmin   = val_ True
             }
      ]
  pure (userId user)


deleteUser :: Int32 -> Pg (Either Text ())
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
  deriving (Generic, Show, Eq)

instance ToJSON ReturnedUser where
  toJSON = genericToJSON defaultOptions
             { fieldLabelModifier = camelTo2 '_' . drop (length ("rUser" :: String)) }

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

getAllUsers :: Integer -> Pg [ReturnedUser]
getAllUsers pageNum = do
  users <- runSelectReturningList $ select $
             all_ (dbUser newsDb)
             & orderBy_ (asc_ . userId)
             & offset_ (20 * (pageNum - 1))
             & limit_ 20
  pure (fmap userToReturned users)
