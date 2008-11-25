module Turbinado.Database.ORM.PostgreSQL where

import Database.HDBC

getPrimaryKeys :: IConnection conn => conn -> [String] -> IO [String, [String]]
getPrimaryKeys conn ts = mapM (\t -> getPrimaryKey conn t >>= \pks -> return (t, pks) ) ts

getPrimaryKey :: IConnection conn => conn -> String -> IO [String]
getPrimaryKey conn t = quickQuery conn (concatenate [
     " SELECT ins.tablename, ins.indexname, i.indkey, a.*"
    ," FROM pg_indexes ins                               "
    ,"   INNER JOIN pg_class c ON ins.indexname = c.relname "
    ,"   INNER JOIN pg_index i ON c.oid = i.indexrelid "
    ,"   INNER JOIN pg_attribute a ON c.oid = a.attrelid "
    ," WHERE ins.tablename = ? AND contype = 'f';"]) [toSql t]
    
getAllForeignKeys :: IConnection conn => conn -> [String] -> IO [(String, String), (String, String)]
getAllForeignKeys conn ts = mapM (\t -> getForeignKeys conn t) ts

getForeignKeys_ :: IConnection conn => conn -> String -> IO [((String, String), (String, String))]
getForeignKeys_ conn t = quickQuery conn (concatenate [
     " SELECT ins.tablename, ins.indexname, i.indkey, a.*"
    ," FROM pg_indexes ins                               "
    ,"   INNER JOIN pg_class c ON ins.indexname = c.relname "
    ,"   INNER JOIN pg_index i ON c.oid = i.indexrelid "
    ,"   INNER JOIN pg_attribute a ON c.oid = a.attrelid "
    ," WHERE ins.tablename = ?;"]) [toSql t]
    
    
{-

INDEX COLUMNS

  select n.nspname as schema_name,
         ct.relname as table_name,
         ci.relname as index_name,
         a.attname as column_name,
         s.i as column_position,
         n2.nspname as opclass_schema,
         o.opcname as opclass_name,
         pg_get_indexdef(ci.oid,s.i,true) as definition
    from pg_index x
         join pg_class ct on (ct.oid = x.indrelid)
         join pg_class ci on (ci.oid = x.indexrelid)
         join pg_namespace n on (n.oid = ct.relnamespace)
         join _pg_sv_keypositions() s(i) on (s.i <= x.indnatts)
         join pg_opclass o on (o.oid = x.indclass[i-1])
         join pg_namespace n2 on (n2.oid = o.opcnamespace)
         left join pg_attribute a on (a.attrelid = ct.oid
                                      and a.attnum = x.indkey[i-1])
   where _pg_sv_table_accessible(n.oid,ct.oid)
     and ct.relkind = 'r' and ci.relkind = 'i';

PRIMARY KEY

  select n.nspname as schema_name,
         c.relname as table_name,
         con.conname as constraint_name,
         con.contype = 'p' as is_primary_key,
         a.attname as column_name,
         s.i as column_position,
         c.oid as table_oid
    from pg_constraint con
         join pg_namespace n on (n.oid = con.connamespace)
         join pg_class c on (c.oid = con.conrelid)
         join _pg_sv_keypositions() s(i)
           on (s.i <= array_upper(con.conkey,1))
         join pg_attribute a on (a.attrelid = c.oid
                                 and a.attnum = con.conkey[i])
   where con.conrelid != 0
     and con.contype in ('p','u')
     and _pg_sv_table_accessible(n.oid,c.oid);


FOREIGN KEYS

  select n1.nspname as foreign_key_schema_name,
         c1.relname as foreign_key_table_name,
         k1.conname as foreign_key_constraint_name,
         c1.oid as foreign_key_table_oid,
         a1.attname as foreign_key_column,
         s.i as column_position,
         n2.nspname as key_schema_name,
         c2.relname as key_table_name,
         c2.oid as key_table_oid,
         a2.attname as key_column
    from pg_constraint k1
         join pg_namespace n1 on (n1.oid = k1.connamespace)
         join pg_class c1 on (c1.oid = k1.conrelid)
         join pg_class c2 on (c2.oid = k1.confrelid)
         join pg_namespace n2 on (n2.oid = c2.relnamespace)
         join _pg_sv_keypositions() s(i)
           on (s.i <= array_upper(k1.conkey,1))
         join pg_attribute a1
           on (a1.attrelid = c1.oid and a1.attnum = k1.conkey[s.i])
         join pg_attribute a2
           on (a2.attrelid = c2.oid and a2.attnum = k1.confkey[s.i])
   where k1.conrelid != 0
     and k1.confrelid != 0
     and k1.contype = 'f'
     and _pg_sv_table_accessible(n1.oid,c1.oid);

-}