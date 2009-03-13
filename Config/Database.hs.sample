module Config.Database (
  databaseConnection,
  ) where

import System.Log.Logger

-- Your favorite HDBC driver
import Database.HDBC.PostgreSQL

import Turbinado.Environment.Types
----------------------------------------------------------------
-- Database connection
----------------------------------------------------------------
-- databaseConnection :: Maybe (IO Connection)
-- databaseConnection = Nothing
databaseConnection = Just $ connectPostgreSQL "host=localhost dbname=turbinado user=turbinado password=turbinado"


