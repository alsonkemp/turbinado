module Turbinado.Database.ORM.Generator where

import qualified Data.Map as M

import Config.Master

type TableName = String
type ColumnName = String
type Column = (SqlColDesc, DependentKeys, Boolean)  -- Boolean == isPrimaryKey
type DependentKeys = [((ColumnName, (TableName, ColumnName))]  -- all columns which are targets of foreign keys

type TableColumn = (TableName, ColumnName)
type TableColumns = M.Map TableColumn Column

generateModels :: FilePath -> IO ()
generateModels cs fp = do conn <- databaseConnection
                          ts <- Database.HDBC.getTables conn
                          ds <- zip ts $ mapM (describeTable conn) ts
                          let tcs = combineTablesColumns ts ds
                          -- TODO: Pull in indices
                          pks <- getPrimaryKeys conn ts
                          let tcs' = combinePrimaryKeys pks tcs 
                          fks <- getForeignKeys t
                          let tcs'' = combineForeignKeys fks tcs'
                          
                          
combineTablesColumns :: [TableName] -> [(ColumnName, SqlColDesc)] -> TableColumns
combineTablesColumsn ts cs =
    M.fromList $ zipWith (\t (c, d) -> ((t,c), (d, [], False)) ) ts cs 

combinePrimaryKeys :: [(TableName, [ColumnName])] -> TableColumns -> TableColumns
combinePrimaryKeys pks tcs =
     foldl (\tcs (t, cs) -> foldl (\c -> M.adjust (\(d,k,_) -> (d, k, True)) (t, c) ) tcs cs) tcs pks

combineForeignKeys :: [(TableColumn, TableColumn)] -> TableColumns -> TableColumns
combineForeignKeys fks tcs = foldl (\tcs' fk -> combineForeignKey fk tcs') tcs fks

combineForeignKey :: (TableColumn, TableColumn) -> TableColumns -> TableColumns
combineForeignKey (parTable, parColumn), ((depTable, depColumn)) tcs =
    let c@(d, k, i) = M.lookup (parTable, parColumn) tcs  in
    M.insert (parTable, parColumn) (d, k `union` (parColumn, (depTable, depColumn)), i) t
