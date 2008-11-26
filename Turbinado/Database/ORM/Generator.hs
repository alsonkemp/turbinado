module Turbinado.Database.ORM.Generator where

import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Database.HDBC

import Config.Master
import Turbinado.Database.ORM.Types
import Turbinado.Database.ORM.Output
import Turbinado.Database.ORM.PostgreSQL

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
                           return $ combineForeignKeyReferences t fks tcs''

combineDescription t ds tcs = M.insert t (cols, []) tcs
  where cols = M.fromList $ 
                map (\(columnName, columnDescription) -> (columnName, (columnDescription,[]))) ds

combinePrimaryKeys :: TableName -> [ColumnName] -> Tables -> Tables
combinePrimaryKeys t pks tcs = M.adjust (\(c, _) -> (c,pks)) t tcs

combineForeignKeyReferences :: TableName -> [(ColumnName, TableName, ColumnName)] -> Tables -> Tables
combineForeignKeyReferences t fks tcs =
    M.adjust
      (\(cs, pks) -> (foldl (worker) cs fks, pks))
        t tcs
  where worker cs (c, tt, tc) = M.adjust (\(cd, deps) -> (cd, [(tt, tc)] `union` deps)) c cs
{-
 - combineTablesColumns :: [TableName] -> [(ColumnName, SqlColDesc)] -> Tables
 - combineTablesColumsn ts cs =
 - M.fromList $ zipWith (\t (c, d) -> (t, (c, [])) ) ts cs 
 -}
