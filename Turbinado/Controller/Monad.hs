module Turbinado.Controller.Monad (
        -- * The 'Controller' Monad
        Controller,
        runController,
        withController,

        get,
        put,
        -- * Functions
        doIO, catch
        ) where

import Control.Exception (catchDyn)

import Control.Monad.State
import Control.Monad.Trans (MonadIO(..))
import Data.Maybe
import Prelude hiding (catch)

import Turbinado.Environment.Types
import Turbinado.Controller.Exception
import Turbinado.Utility.General


--------------------------------------------------------------
-- The Controller Monad

-- | The Controller monad is a state wrapper around
-- the IO monad.

type Controller = StateT Environment IO


-- | Runs a Controller computation in a particular environment. Since Controller wraps the IO monad,
-- the result of running it will be an IO computation.
runController :: Controller () -> Environment -> IO Environment
runController c e = (execStateT c) e

withController :: (Environment -> Environment) -> Controller a -> Controller a
withController = withStateT

-- | Execute an IO computation within the Controller monad.
doIO :: IO a -> Controller a
doIO = liftIO

-----------------------------------------------------------------------
-- Exception handling

-- | Catch a user-caused exception.
catch :: Controller a -> (Exception -> Controller a) -> Controller a
catch (StateT f) handler = StateT $ \e ->
        f e `catchDyn` (\ex -> (let (StateT g) = handler ex
                                   in g e))

