module Turbinado.Database.ORM.Types where

import qualified Data.Map as M
import Database.HDBC

type TypeName = String

--
-- * Types for the Table descriptions
--
type Tables = M.Map TableName (Columns, PrimaryKey)
type TableName = String
type Columns = M.Map ColumnName ColumnDesc
type ColumnName = String
type PrimaryKey = [ColumnName]
type ColumnDesc = (SqlColDesc, ForeignKeyReferences, HasDefault)
type HasDefault = Bool
type ForeignKeyReferences = [(TableName, ColumnName)]  -- all columns which are targets of foreign keys

