module Queries.Util where


import Data.Maybe (fromMaybe)

import Database.Beam
import Database.Beam.Postgres

import BeamSchema


type DbWith = With Postgres NewsDb
type DbQ = Q Postgres NewsDb
type DbQExpr = QExpr Postgres
type T2 s a b = (DbQExpr s a, DbQExpr s b)

maybeAssignment :: Maybe a -> (a -> QAssignment Postgres s) -> QAssignment Postgres s
maybeAssignment x f = fromMaybe mempty (fmap f x)
