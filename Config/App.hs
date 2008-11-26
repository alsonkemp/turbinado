module Config.App where

import System.Log.Logger

----------------------------------------------------------------
-- Environment settings
----------------------------------------------------------------
applicationPath = ""
applicationHost = "localhost:8080"

----------------------------------------------------------------
-- RequestHandler Filter List additions
----------------------------------------------------------------
customPreFilters  = []
customPostFilters = []


----------------------------------------------------------------
-- Logging
----------------------------------------------------------------
logLevel = ERROR -- DEBUG < INFO < NOTICE < WARNING < ERROR < CRITICAL < ALERT < EMERGENCY 


