module Queries.Commentary where


import Data.Int (Int32)
import Data.Text (Text)

import Database.Beam
import Database.Beam.Postgres

import BeamSchema


data CreateCommentary = CreateCommentary
  { cCommentaryUserId :: Int32
  , cCommentaryPostId :: Int32
  , cCommentaryContent :: Text
  }

createCommentary :: CreateCommentary -> Pg ()
createCommentary ct =
  runInsert $ insert (dbCommentary newsDb) $
    insertExpressions
      [ Commentary
          { commentaryId      = default_
          , commentaryUserId  = val_ (cCommentaryUserId ct)
          , commentaryPostId  = val_ (cCommentaryPostId ct)
          , commentaryContent = val_ (cCommentaryContent ct)
          }
      ]
