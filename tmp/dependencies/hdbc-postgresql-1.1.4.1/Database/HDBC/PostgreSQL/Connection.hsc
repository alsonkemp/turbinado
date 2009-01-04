-- -*- mode: haskell; -*-
{-# CFILES hdbc-postgresql-helper.c #-}
-- Above line for hugs
{-
Copyright (C) 2005-2006 John Goerzen <jgoerzen@complete.org>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-}

module Database.HDBC.PostgreSQL.Connection
	(connectPostgreSQL, Impl.Connection())
 where

import Database.HDBC.Types
import Database.HDBC
import Database.HDBC.DriverUtils
import Database.HDBC.ColTypes
import qualified Database.HDBC.PostgreSQL.ConnectionImpl as Impl
import Database.HDBC.PostgreSQL.Types
import Database.HDBC.PostgreSQL.Statement
import Database.HDBC.PostgreSQL.PTypeConv
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Database.HDBC.PostgreSQL.Utils
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Word
import Data.Maybe
import Control.Concurrent.MVar

#include <libpq-fe.h>
#include <pg_config.h>

{- | Connect to a PostgreSQL server.

See <http://www.postgresql.org/docs/8.1/static/libpq.html#LIBPQ-CONNECT> for the meaning
of the connection string. -}
connectPostgreSQL :: String -> IO Impl.Connection
connectPostgreSQL args = withCString args $
  \cs -> do ptr <- pqconnectdb cs
            status <- pqstatus ptr
            wrappedptr <- wrapconn ptr nullPtr
            fptr <- newForeignPtr pqfinishptr wrappedptr
            case status of
                     #{const CONNECTION_OK} -> mkConn args fptr
                     _ -> raiseError "connectPostgreSQL" status ptr

-- FIXME: environment vars may have changed, should use pgsql enquiries
-- for clone.
mkConn :: String -> Conn -> IO Impl.Connection
mkConn args conn = withConn conn $
  \cconn -> 
    do children <- newMVar []
       begin_transaction conn children
       protover <- pqprotocolVersion cconn
       serverver <- pqserverVersion cconn
       let clientver = #{const_str PG_VERSION}
       let rconn = Impl.Connection {
                            Impl.disconnect = fdisconnect conn children,
                            Impl.commit = fcommit conn children,
                            Impl.rollback = frollback conn children,
                            Impl.run = frun conn children,
                            Impl.prepare = newSth conn children,
                            Impl.clone = connectPostgreSQL args,
                            Impl.hdbcDriverName = "postgresql",
                            Impl.hdbcClientVer = clientver,
                            Impl.proxiedClientName = "postgresql",
                            Impl.proxiedClientVer = show protover,
                            Impl.dbServerVer = show serverver,
                            Impl.dbTransactionSupport = True,
                            Impl.getTables = fgetTables conn children,
                            Impl.describeTable = fdescribeTable conn children}
       quickQuery rconn "SET client_encoding TO utf8;" []
       return rconn

--------------------------------------------------
-- Guts here
--------------------------------------------------

begin_transaction :: Conn -> ChildList -> IO ()
begin_transaction o children = frun o children "BEGIN" [] >> return ()

frun o children query args =
    do sth <- newSth o children query
       res <- execute sth args
       finish sth
       return res

fcommit o cl = do frun o cl "COMMIT" []
                  begin_transaction o cl
frollback o cl =  do frun o cl "ROLLBACK" []
                     begin_transaction o cl

fgetTables conn children =
    do sth <- newSth conn children "select table_name from information_schema.tables where table_schema = 'public'"
       execute sth []
       res1 <- fetchAllRows' sth
       let res = map fromSql $ concat res1
       return $ seq (length res) res

fdescribeTable o cl table = fdescribeSchemaTable o cl Nothing table

fdescribeSchemaTable :: Conn -> ChildList -> Maybe String -> String -> IO [(String, SqlColDesc)]
fdescribeSchemaTable o cl maybeSchema table =
    do sth <- newSth o cl 
              ("SELECT attname, atttypid, attlen, format_type(atttypid, atttypmod), attnotnull " ++
               "FROM pg_attribute, pg_class, pg_namespace ns " ++
               "WHERE relname = ? and attnum > 0 and attisdropped IS FALSE " ++
               (if isJust maybeSchema then "and ns.nspname = ? " else "") ++
               "and attrelid = pg_class.oid and relnamespace = ns.oid order by attnum")
       let params = toSql table : (if isJust maybeSchema then [toSql $ fromJust maybeSchema] else [])
       execute sth params
       res <- fetchAllRows' sth
       return $ map desccol res
    where
      desccol [attname, atttypid, attlen, formattedtype, attnotnull] =
          (fromSql attname, 
           colDescForPGAttr (fromSql atttypid) (fromSql attlen) (fromSql formattedtype) (fromSql attnotnull == False))
      desccol x =
          error $ "Got unexpected result from pg_attribute: " ++ show x
         

fdisconnect conn mchildren = 
    do closeAllChildren mchildren
       withRawConn conn $ pqfinish

foreign import ccall unsafe "libpq-fe.h PQconnectdb"
  pqconnectdb :: CString -> IO (Ptr CConn)

foreign import ccall unsafe "hdbc-postgresql-helper.h wrapobjpg"
  wrapconn :: Ptr CConn -> Ptr WrappedCConn -> IO (Ptr WrappedCConn)

foreign import ccall unsafe "libpq-fe.h PQstatus"
  pqstatus :: Ptr CConn -> IO #{type ConnStatusType}

foreign import ccall unsafe "hdbc-postgresql-helper.h PQfinish_app"
  pqfinish :: Ptr WrappedCConn -> IO ()

foreign import ccall unsafe "hdbc-postgresql-helper.h &PQfinish_finalizer"
  pqfinishptr :: FunPtr (Ptr WrappedCConn -> IO ())

foreign import ccall unsafe "libpq-fe.h PQprotocolVersion"
  pqprotocolVersion :: Ptr CConn -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQserverVersion"
  pqserverVersion :: Ptr CConn -> IO CInt
