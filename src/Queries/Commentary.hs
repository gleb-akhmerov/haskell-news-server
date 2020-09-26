module Queries.Commentary where


import Control.Monad.Trans.Except (runExceptT)
import Data.Int (Int32)
import Data.Text (Text)

import Data.Aeson
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import BeamSchema
import Queries.Util


data CreateCommentary = CreateCommentary
  { cCommentaryUserId :: Int32
  , cCommentaryPostId :: Int32
  , cCommentaryContent :: Text
  }
  deriving (Generic, Show)

instance FromJSON CreateCommentary where
  parseJSON = genericParseJSON defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop (length "cCommentary") }

createCommentary :: CreateCommentary -> Pg Int32
createCommentary ct = do
  [commentary] <- runInsertReturningList $ insert (dbCommentary newsDb) $
    insertExpressions
      [ Commentary
          { commentaryId      = default_
          , commentaryUserId  = val_ (cCommentaryUserId ct)
          , commentaryPostId  = val_ (cCommentaryPostId ct)
          , commentaryContent = val_ (cCommentaryContent ct)
          }
      ]
  pure (commentaryId commentary)


deleteCommentary :: Int32 -> Pg (Either String ())
deleteCommentary dCommentaryId = runExceptT $ do
  makeSureEntityExists "Commentary" (dbCommentary newsDb) commentaryId dCommentaryId
  runDelete $ delete (dbCommentary newsDb)
    (\a -> commentaryId a ==. val_ dCommentaryId)


data ReturnedCommentary = ReturnedCommentary
  { rCommentaryId :: Int32
  , rCommentaryUserId :: Int32
  , rCommentaryPostId :: Int32
  , rCommentaryContent :: Text
  }
  deriving (Generic, Show)

instance ToJSON ReturnedCommentary where
  toJSON = genericToJSON defaultOptions
             { fieldLabelModifier = camelTo2 '_' . drop (length "rCommentary") }

commentaryToReturned :: Commentary -> ReturnedCommentary
commentaryToReturned c =
  ReturnedCommentary
    { rCommentaryId      = commentaryId c
    , rCommentaryUserId  = commentaryUserId c
    , rCommentaryPostId  = commentaryPostId c
    , rCommentaryContent = commentaryContent c
    }

getCommentary :: Int32 -> Pg (Maybe ReturnedCommentary)
getCommentary gCommentaryId = do
  mCommentary <- runSelectReturningOne $ select $
               filter_ (\a -> commentaryId a ==. val_ gCommentaryId)
                       (all_ (dbCommentary newsDb))
  pure (fmap commentaryToReturned mCommentary)

getAllCommentaries :: Pg [ReturnedCommentary]
getAllCommentaries = do
  commentaries <- runSelectReturningList $ select $ all_ (dbCommentary newsDb)
  pure (fmap commentaryToReturned commentaries)
