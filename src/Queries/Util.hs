{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Queries.Util where


import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Int (Int32)
import Data.Maybe (fromMaybe, isNothing)

import Database.Beam
import Database.Beam.Postgres

import BeamSchema


type DbWith = With Postgres NewsDb
type DbQ = Q Postgres NewsDb
type DbQExpr = QExpr Postgres
type T2 s a b = (DbQExpr s a, DbQExpr s b)

maybeAssignment :: Maybe a -> (a -> QAssignment Postgres s) -> QAssignment Postgres s
maybeAssignment x f = fromMaybe mempty (fmap f x)

makeSureEntityExists
  :: (FromBackendRow Postgres (table Identity), (Table table))
  => String
  -> DatabaseEntity Postgres NewsDb (TableEntity table)
  -> (table (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Int32)
  -> Int32
  -> ExceptT String Pg ()
makeSureEntityExists entityName table getEntityId entityId = do
  mEntity <- runSelectReturningOne $ select $
    filter_ (\e -> getEntityId e ==. val_ entityId)
            (all_ table)
  when (isNothing mEntity) $
    throwE $ entityName ++ " with id doesn't exist: " ++ show entityId

maybeDo :: Applicative f => (x -> f ()) -> Maybe x -> f ()
maybeDo f maybeX =
  case maybeX of
    Nothing -> pure ()
    Just x  -> f x
