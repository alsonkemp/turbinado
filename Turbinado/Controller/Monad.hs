module Turbinado.Controller.Monad (
        -- * The 'Controller' Monad
        Controller,
        runController,
        withController,

        get,
        put,
        -- * Functions
        liftIO, catch
        ) where

import Control.OldException (catchDyn)

import Control.Monad.State
import Control.Monad.Trans (MonadIO(..), liftIO)
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

instance HasEnvironment Controller where
  getEnvironment = get
  setEnvironment = put

-- | Runs a Controller computation in a particular environment. Since Controller wraps the IO monad,
-- the result of running it will be an IO computation.
runController :: Controller () -> Environment -> IO Environment
runController c e = (execStateT c) e

withController :: (Environment -> Environment) -> Controller a -> Controller a
withController = withStateT

-----------------------------------------------------------------------
-- Exception handling

-- | Catch a user-caused exception.
catch :: Controller a -> (Exception -> Controller a) -> Controller a
catch (StateT f) handler = StateT $ \e ->
        f e `catchDyn` (\ex -> (let (StateT g) = handler ex
                                   in g e))

