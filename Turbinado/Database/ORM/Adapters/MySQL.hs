module Turbinado.Database.ORM.Adapters.MySQL where

import Data.List
import Database.HDBC

import Turbinado.Database.ORM.Types
import qualified Turbinado.Database.ORM.Adapters.Types as T

ormAdapter = T.baseAdapter {
                 T.getPrimaryKeys = getPrimaryKeys,
                 T.getForeignKeyReferences = getForeignKeyReferences,
                 T.getDefaultColumns = getDefaultColumns
                 }

--
-- * Functions for extracting information from the database
--

getPrimaryKeys :: IConnection conn => conn -> String -> IO [String]
getPrimaryKeys conn t = 
    do rs <- quickQuery conn ("show columns from " ++ t) []
       return $ map (\r -> fromSql $ r !! 0) rs

getForeignKeyReferences :: IConnection conn => conn -> String -> IO [(String, String, String)]
getForeignKeyReferences conn t = return []   

getDefaultColumns:: IConnection conn => conn -> String -> IO [String]
getDefaultColumns conn t = 
    do rs <- quickQuery conn ("show columns from " ++ t) []
       return $ map (\r -> fromSql $ r !! 0) rs

