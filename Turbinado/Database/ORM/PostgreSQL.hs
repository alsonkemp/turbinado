module Turbinado.Database.ORM.PostgreSQL where

import Database.HDBC

getPrimaryKeys :: IConnection conn => conn -> String -> [String]
getPrimaryKeys conn t = quickQuery conn (concatenate [
     " SELECT ins.tablename, ins.indexname, i.indkey, a.*"
    ," FROM pg_indexes ins                               "
    ,"   INNER JOIN pg_class c ON ins.indexname = c.relname "
    ,"   INNER JOIN pg_index i ON c.oid = i.indexrelid "
    ,"   INNER JOIN pg_attribute a ON c.oid = a.attrelid "
    ," WHERE ins.tablename = '" ++ t ++ "' AND contype = 'f';"]) []
    
getForeignKeys :: IConnection conn => conn -> String -> [String]
getForeignKeys conn t = quickQuery conn (concatenate [
     " SELECT ins.tablename, ins.indexname, i.indkey, a.*"
    ," FROM pg_indexes ins                               "
    ,"   INNER JOIN pg_class c ON ins.indexname = c.relname "
    ,"   INNER JOIN pg_index i ON c.oid = i.indexrelid "
    ,"   INNER JOIN pg_attribute a ON c.oid = a.attrelid "
    ," WHERE ins.tablename = '" ++ t ++ "';"]) []