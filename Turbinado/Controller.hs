module Turbinado.Controller (
        getEnvironment,
        evalController,
        -- limited export from Turbinado.Controller.Monad
        Controller,
        runController,
        -- * Functions
        doIO, catch,

        module Turbinado.Environment,
        module Turbinado.Environment.CodeStore,
        module Turbinado.Environment.Request,
        module Turbinado.Environment.Response,
        ) where

import Control.Exception (catchDyn)
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans (MonadIO(..))
import qualified Network.HTTP as HTTP
import Prelude hiding (catch)

import Turbinado.Environment
import Turbinado.Environment.Request
import Turbinado.Environment.Response
import Turbinado.Controller.Monad
import Turbinado.Environment.CodeStore
import Turbinado.Utility.General


evalController :: Controller () -> EnvironmentFilter
evalController p e = runController p e


--
-- * Environment functions
--

getEnvironment :: Controller Environment
getEnvironment = get
