module Turbinado.Controller (
        -- limited export from Turbinado.Controller.Monad
        Controller,
        runController,
        -- * Functions
        liftIO, catch,

        redirectTo,
        -- * Database
        quickQuery,
        quickQuery',
        run,
        HDBC.SqlValue(..),
        HDBC.SqlType(..),

        module Data.Maybe,

        module Turbinado.Environment.CodeStore,
        module Turbinado.Environment.Header,
        module Turbinado.Environment.Logger,
        module Turbinado.Environment.Params,
        module Turbinado.Environment.Request,
        module Turbinado.Environment.Response,
        module Turbinado.Environment.Settings,
        module Turbinado.Environment.Types,
        module Turbinado.Environment.ViewData
        ) where

import Control.Exception (catchDyn)
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans (MonadIO(..))
import Data.Maybe
import qualified Network.HTTP as HTTP
import Prelude hiding (catch)
import qualified Database.HDBC as HDBC

import Turbinado.Environment.CodeStore
import Turbinado.Environment.Database
import Turbinado.Environment.Header
import Turbinado.Environment.Logger
import Turbinado.Environment.Params
import Turbinado.Environment.Request
import Turbinado.Environment.Response
import Turbinado.Environment.Settings
import Turbinado.Environment.Types
import Turbinado.Environment.ViewData
import Turbinado.Controller.Monad
import Turbinado.Utility.General
import Turbinado.Server.StandardResponse

-- evalController :: Controller () -> Environment -> IO Environment
-- evalController p = runController p e

--
-- * Helper functions
--

redirectTo :: String -> Controller ()
redirectTo l = redirectResponse l

--
-- * Database functions
--

quickQuery :: String -> [HDBC.SqlValue] -> Controller [[HDBC.SqlValue]]
quickQuery s vs = do e <- get
                     let c = fromJust $ getDatabase e
                     liftIO $ HDBC.handleSqlError $ HDBC.quickQuery c s vs

quickQuery' :: String -> [HDBC.SqlValue] -> Controller [[HDBC.SqlValue]]
quickQuery' s vs = do e <- get
                      let c = fromJust $ getDatabase e
                      liftIO $ HDBC.handleSqlError $ HDBC.quickQuery' c s vs

run :: String -> [HDBC.SqlValue] -> Controller Integer
run s vs    = do e <- get
                 let c = fromJust $ getDatabase e
                 liftIO $ HDBC.handleSqlError $  HDBC.run c s vs

