module Turbinado.Database.ORM.Generator (
  generateModels
) where

import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Database.HDBC

import Config.Database
import Turbinado.Database.ORM.Types
import Turbinado.Database.ORM.Output
import Turbinado.Database.ORM.PostgreSQL

-- | Outputs ORM models to App/Models.  User configurable files
-- are in App/Models.  Machine generated files are in App/Models/Bases.
generateModels ::  IO ()
generateModels = do conn <- fromJust databaseConnection
                    ts <- getTables conn
                    -- TODO: Pull in indices
                    tcs <- foldM (buildTable conn) (M.empty) ts
                    writeModels tcs
                          
                          
buildTable conn tcs t = do ds <- describeTable conn t
                           let tcs'  = combineDescription t ds tcs
                           pks <- getPrimaryKeys conn t
                           let tcs'' = combinePrimaryKeys t pks tcs'
                           fks <- getForeignKeyReferences conn t
                           let tcs''' = combineForeignKeyReferences t fks tcs''
                           hds <- getDefaultColumns conn t
                           return $ combineDefaultColumns t hds tcs'''

combineDescription t ds tcs = M.insert t (cols, []) tcs
  where cols = M.fromList $ 
                map (\(columnName, columnDescription) -> (columnName, (columnDescription,[], False))) ds

combinePrimaryKeys :: TableName -> [ColumnName] -> Tables -> Tables
combinePrimaryKeys t pks tcs = M.adjust (\(c, _) -> (c,pks)) t tcs

combineForeignKeyReferences :: TableName -> [(ColumnName, TableName, ColumnName)] -> Tables -> Tables
combineForeignKeyReferences t fks tcs =
    M.adjust
      (\(cs, pks) -> (foldl (worker) cs fks, pks))
        t tcs
  where worker cs (c, tt, tc) = M.adjust (\(cd, deps, hd) -> (cd, [(tt, tc)] `union` deps, hd)) c cs

combineDefaultColumns :: TableName -> [ColumnName] -> Tables -> Tables
combineDefaultColumns t hds tcs =
    M.adjust
      (\(cs, pks) -> (foldl (worker) cs hds, pks))
        t tcs
  where worker cs hd = M.adjust (\(cd, deps, _) -> (cd, deps, True)) hd cs


