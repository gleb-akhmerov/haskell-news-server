module Queries.Commentary where


import Control.Monad.Trans.Except (runExceptT)
import Data.Function ((&))
import Data.Int (Int32)
import Data.Text (Text)

import Data.Aeson
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import BeamSchema
import Queries.Util


data CreateCommentary = CreateCommentary
  { cCommentaryContent :: Text
  }
  deriving (Generic, Show)

instance FromJSON CreateCommentary where
  parseJSON = genericParseJSON defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop (length "cCommentary") }

createCommentary :: Int32 -> Int32 -> CreateCommentary -> Pg (Either String Int32)
createCommentary cCommentaryUserId cCommentaryPostId ct = runExceptT $ do
  makeSureEntityExists "User" (dbUser newsDb) userId cCommentaryUserId
  makeSureEntityExists "Post" (dbPost newsDb) postId cCommentaryPostId
  [commentary] <- runInsertReturningList $ insert (dbCommentary newsDb) $
    insertExpressions
      [ Commentary
          { commentaryId      = default_
          , commentaryUserId  = val_ cCommentaryUserId
          , commentaryPostId  = val_ cCommentaryPostId
          , commentaryContent = val_ (cCommentaryContent ct)
          }
      ]
  pure (commentaryId commentary)


deleteCommentary :: Int32 -> Int32 -> Pg (Either String ())
deleteCommentary dPostId dCommentaryId = runExceptT $ do
  makeSureEntityExists "Post" (dbPost newsDb) postId dPostId
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

getPostCommentaries :: Int32 -> Integer -> Pg (Maybe [ReturnedCommentary])
getPostCommentaries gPostId pageNum = fmap rightToMaybe $ runExceptT $ do
  makeSureEntityExists "Post" (dbPost newsDb) postId gPostId
  comments <- runSelectReturningList $ select $ do
                post <- filter_ (\p -> postId p ==. val_ gPostId)
                                (all_ (dbPost newsDb))
                commentary <- join_ (dbCommentary newsDb)
                                    (\c -> commentaryPostId c ==. postId post)
                pure commentary
                & orderBy_ (asc_ . commentaryId)
                & offset_ (20 * (pageNum - 1))
                & limit_ 20
  pure (fmap commentaryToReturned comments)
