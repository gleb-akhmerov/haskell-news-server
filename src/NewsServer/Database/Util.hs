{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module NewsServer.Database.Util where


import Control.Monad (unless, when)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Int (Int32)
import Data.Maybe (fromMaybe, isNothing)
import Data.Vector (Vector)

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax (PgExpressionSyntax(..), emit, pgParens)
import Database.Beam.Query.Internal (QAgg)

import NewsServer.Database.BeamSchema


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
    join_ table (\e -> getEntityId e ==. val_ entityId)
  when (isNothing mEntity) $
    throwE $ entityName ++ " with id doesn't exist: " ++ show entityId


makeSureNoReferenceExists
  :: (FromBackendRow Postgres (table Identity), (Table table))
  => String
  -> String
  -> DatabaseEntity Postgres NewsDb (TableEntity table)
  -> (table (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Int32)
  -> (table Identity -> Int32)
  -> Int32
  -> ExceptT String Pg ()
makeSureNoReferenceExists entityName referencingEntitiesName referencingTable getEntityId getReferencingEntityId entityId = do
  referencingEntities <- runSelectReturningList $ select $
    join_ referencingTable (\r -> getEntityId r ==. val_ entityId)
  unless (null referencingEntities) $
    throwE $ entityName ++ " with id " ++ show entityId ++ " is referenced by " ++ referencingEntitiesName ++ " with ids: " ++ show (fmap getReferencingEntityId referencingEntities)


makeSureNoMaybeReferenceExists
  :: (FromBackendRow Postgres (table Identity), (Table table))
  => String
  -> String
  -> DatabaseEntity Postgres NewsDb (TableEntity table)
  -> (table (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope (Maybe Int32))
  -> (table Identity -> Int32)
  -> Int32
  -> ExceptT String Pg ()
makeSureNoMaybeReferenceExists entityName referencingEntitiesName referencingTable getEntityId getReferencingEntityId entityId = do
  referencingEntities <- runSelectReturningList $ select $
    join_ referencingTable (\r -> getEntityId r ==. just_ (val_ entityId))
  unless (null referencingEntities) $
    throwE $ entityName ++ " with id " ++ show entityId ++ " is referenced by " ++ referencingEntitiesName ++ " with ids: " ++ show (fmap getReferencingEntityId referencingEntities)


makeSureNoReferenceExistsMtm
  :: (FromBackendRow Postgres (mtmTable Identity), (Table mtmTable))
  => (FromBackendRow Postgres (refTable Identity), (Table refTable))
  => String
  -> String
  -> DatabaseEntity Postgres NewsDb (TableEntity mtmTable)
  -> (mtmTable (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Int32)
  -> (mtmTable (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Int32)
  -> DatabaseEntity Postgres NewsDb (TableEntity refTable)
  -> (refTable (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Int32)
  -> (refTable Identity -> Int32)
  -> Int32
  -> ExceptT String Pg ()
makeSureNoReferenceExistsMtm entityName referencingEntitiesName mtmTable mtmEntityId mtmReferencingId refTable getReferenceId getReferenceId' entityId = do
  referencingEntities <- runSelectReturningList $ select $ do
    mtm <- join_ mtmTable
                 (\mtm -> mtmEntityId mtm ==. val_ entityId)
    referencingEntity <- join_ refTable
                               (\r -> getReferenceId r ==. mtmReferencingId mtm)
    pure referencingEntity
  unless (null referencingEntities) $
    throwE $ entityName ++ " with id " ++ show entityId ++ " is referenced by " ++ referencingEntitiesName ++ " with ids: " ++ show (fmap getReferenceId' referencingEntities)


maybeDo :: Applicative f => (x -> f ()) -> Maybe x -> f ()
maybeDo f maybeX =
  case maybeX of
    Nothing -> pure ()
    Just x  -> f x


rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just


arrayLen :: QGenExpr ctxt Postgres s (Vector v) -> QGenExpr ctxt Postgres s Int32
arrayLen = customExpr_ (\arr -> "array_length(" <> arr <> ", 1)")


removeNullsAgg :: QAgg Postgres s (Vector (Maybe a))
               -> QAgg Postgres s (Vector a)
removeNullsAgg (QExpr a) =
  QExpr $ \tbl ->
  PgExpressionSyntax $
    emit "array_remove" <>
    pgParens (fromPgExpression (a tbl) <> emit ", NULL")
