module Queries.User where


import Data.ByteString (ByteString)
import Data.Text (Text)

import Database.Beam
import Database.Beam.Postgres

import BeamSchema


data CreateUser = CreateUser
  { cUserFirstName :: Text
  , cUserLastName :: Text
  , cUserAvatar :: ByteString
  , cUserIsAdmin :: Bool
  }

createUser :: CreateUser -> Pg ()
createUser cu =
  runInsert $ insert (dbUser newsDb) $
    insertExpressions
      [ User { userId        = default_
             , userFirstName = val_ (cUserFirstName cu)
             , userLastName  = val_ (cUserLastName cu)
             , userAvatar    = val_ (cUserAvatar cu)
             , userCreatedAt = now_
             , userIsAdmin   = val_ (cUserIsAdmin cu)
             }
      ]
