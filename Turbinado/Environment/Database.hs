module Turbinado.Environment.Database (
        addDatabaseToEnvironment,
        Connection
        ) where

import Data.Typeable
import Data.Dynamic
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Maybe
import qualified Database.HDBC as HDBC
import Database.HDBC (IConnection)

import Config.Master
import Turbinado.Controller.Monad
import Turbinado.Environment.Types


addDatabaseToEnvironment :: Controller ()
addDatabaseToEnvironment = do e <- get
                              case databaseConnection of
                                Nothing   -> return ()
                                Just conn -> do c <- doIO $ conn 
                                                put $ e {getDatabase = Just c}

