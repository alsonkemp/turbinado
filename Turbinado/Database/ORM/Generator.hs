module Turbinado.Database.ORM.Generator (
  generateModels
) where

import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Database.HDBC
import System.Directory
import System.FilePath

import Config.Database
import Turbinado.Database.ORM.Common
import Turbinado.Database.ORM.Types
import Turbinado.Database.ORM.Adapters.Types

-- | Outputs ORM models to App/Models.  User configurable files
-- are in App/Models.  Machine generated files are in App/Models/Bases.
generateModels ::  AdapterType -> IO ()
generateModels a = do conn <- fromJust databaseConnection
                      ts   <- getTables conn
                      -- TODO: Pull in indices
                      putStr "Pulling schemas from the database: "
                      tcs <- foldM (buildTable a conn) (M.empty) ts
                      putStr   "\n\nWriting models: "
                      writeModels a tcs
                      putStrLn "\n"

                          
buildTable a conn tcs t = do putStr $ t ++ ", "
                             ds <- describeTable conn t
                             let tcs'  = combineDescription t ds tcs
                             pks <- (getPrimaryKeys a) conn t
                             let tcs'' = combinePrimaryKeys t pks tcs'
                             fks <- (getForeignKeyReferences a) conn t
                             let tcs''' = combineForeignKeyReferences t fks tcs''
                             hds <- (getDefaultColumns a) conn t
                             return $ combineDefaultColumns t hds tcs'''

writeModels a ts = 
  do writeFile "App/Models/Bases/Common.hs" (generateCommon a)
     mapM_ (\(t, (cs, pk)) -> 
                 let typeName = (toType t) in
                 do putStr $ t ++ ", "
                    e <- doesFileExist (joinPath ["App/Models", typeName ++ ".hs"])
                    when (not e) (writeFile (joinPath ["App/Models", typeName++".hs"]) ((generateModelFile a) typeName) ) 
                    writeFile (joinPath ["App/Models/Bases", typeName ++ "Type.hs"]) ((generateType a) t typeName pk ts cs)
                    writeFile (joinPath ["App/Models/Bases", typeName ++ "Functions.hs"]) ((generateFunctions a) t typeName pk ts cs)
                    writeFile (joinPath ["App/Models/Bases", typeName ++ "Relations.hs"]) ((generateRelations a) t typeName pk ts cs)
           ) $ M.toList ts


--
-- * Functions to build the relational mappings
--
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


