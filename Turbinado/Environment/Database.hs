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


addDatabaseToEnvironment :: (HasEnvironment m) => m ()
addDatabaseToEnvironment = do e <- getEnvironment
                              case databaseConnection of
                                Nothing   -> return ()
                                Just conn -> do c <- liftIO $ conn 
                                                setEnvironment $ e {getDatabase = Just c}

