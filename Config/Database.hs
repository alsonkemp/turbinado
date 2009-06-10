module Config.Database (
  databaseConnection,
  ormAdapter
  ) where

import System.Log.Logger
import Database.HDBC

-- Your favorite HDBC driver
import Database.HDBC.ODBC

import qualified Turbinado.Database.ORM.Adapters.MySQL as M
import Turbinado.Environment.Types
----------------------------------------------------------------
-- Database connection
----------------------------------------------------------------
databaseConnection :: Maybe (IO ConnWrapper)
databaseConnection = Nothing
--databaseConnection = Just $ connectPostgreSQL "host=localhost dbname=turbinado user=turbinado password=turbinado"
--databaseConnection = Just $ connectODBC "DSN=krugi_development;USER=root;password=passw0rd"

ormAdapter = M.ormAdapter

