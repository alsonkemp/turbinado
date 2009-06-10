module Turbinado.Database.ORM.Adapters.Types where

import qualified Data.Map as M
import Database.HDBC

import Turbinado.Database.ORM.Types
import qualified Turbinado.Database.ORM.Adapters.Common as C

baseAdapter   = AdapterType {
                     getPrimaryKeys = error "getPrimaryKeys must be overridden by an adapter",
                     getForeignKeyReferences = error "getForeignKeyReferences must be overridden by an adapter",
                     getDefaultColumns = error "getDefaultColumns must be overridden by an adapter",
                     generateCommon = C.generateCommon,
                     generateModelFile = C.generateModelFile,
                     generateType = C.generateType,
                     generateFunctions = C.generateFunctions,
                     generateRelations = C.generateRelations                     
                     }

data AdapterType = AdapterType {
                     getPrimaryKeys :: IConnection conn => conn -> String -> IO [String],
                     getForeignKeyReferences :: IConnection conn => conn -> String -> IO [(String, String, String)],
                     getDefaultColumns :: IConnection conn => conn -> String -> IO [String],
                     generateCommon :: String,
                     generateModelFile :: String -> String,
                     generateType :: TableName -> TypeName -> PrimaryKey -> Tables -> Columns -> String,
                     generateFunctions :: TableName -> TypeName -> PrimaryKey -> Tables -> Columns -> String,
                     generateRelations :: TableName -> TypeName -> PrimaryKey -> Tables -> Columns -> String                     
                     }
