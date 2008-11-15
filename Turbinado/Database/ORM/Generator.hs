module Turbinado.Database.ORM.Generator where


import qualified Data.Map as M


type ConnectionString = String
type TableName = String
type ColumnName = String
type Column = (SqlColDesc, DependentKeys, Boolean)  -- Boolean == isPrimaryKey
type DependentKeys = [(TableName, ColumnName)]  -- all columns which are targets of foreign keys

type TableColumn = (TableName, ColumnName)
type TableColumns = M.Map TableColumn Column

generateModels :: FilePath -> IO ()
generateModels cs fp = do conn <- openDBConnection
                          ts <- Database.HDBC.getTables conn
                          ds <- zip ts $ mapM (describeTable conn) ts
                          let tcs = combineTablesColumns ts ds
                          pks <- getPrimaryKeys conn t
                          let tcs' = combinePrimaryKeys tcs pks
                          fks <- getForeignKeys t
                          let tcs'' = foldl
                          
                          
combineTablesColumns :: [TableName] -> [(ColumnName, SqlColDesc)] -> TableColumn
combineTablesColumsn ts cs =
    M.fromList $ zipWith (\t (c, d) -> ((t,c), (d, [], False)) ) ts cs 

combinePrimaryKeys :: [(TableName, [ColumnName])] -> TableColumns -> TableColumns
combinePrimaryKeys pks tcs =
     foldl (\tcs (t, cs) -> foldl (\c -> M.adjust (\(d,k,_) -> (d, k, True)) (t, c) ) tcs cs) tcs pks

addDependentKey :: (TableColumn, TableColumn) -> TableColumns -> TableColumns
addDependentKey (parTable, parColumn), ((depTable, depColumn)) t =
    let c@(d, k, i) = M.lookup (parTable, parColumn) t  in
    M.insert (parTable, parColumn) (d, k `union` (depTable, depColumn), i)
