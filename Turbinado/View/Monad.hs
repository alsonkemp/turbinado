module Turbinado.View.Monad (
        -- * The 'View' Monad
        View,
        runView,
        get,
        put,
        -- * Functions
        liftIO, catch
        ) where

import Control.OldException (catchDyn)

import Control.Monad.State
import Control.Monad.Trans (MonadIO(..), liftIO)
import Data.Maybe
import qualified Network.HTTP as HTTP
import Prelude hiding (catch)

import Turbinado.Environment.Types
import Turbinado.View.Exception
import Turbinado.Utility.General


--------------------------------------------------------------
-- The View Monad

-- |The View monad is a wrapper around
-- the IO monad, 
type View = StateT Environment IO

-- |Runs a View computation in a particular environment. Since View wraps the IO monad,
-- the result of running it will be an IO computation.
runView :: View a -> Environment -> IO (a, Environment)
runView p e = runStateT  p e

-----------------------------------------------------------------------
-- Exception handling

-- |Catch a user-caused exception.
catch :: View a -> (Exception -> View a) -> View a
catch (StateT f) handler = StateT $ \e ->
        f e `catchDyn` (\ex -> (let (StateT g) = handler ex
                                   in g e))

