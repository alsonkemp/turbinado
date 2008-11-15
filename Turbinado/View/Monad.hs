module Turbinado.View.Monad (
        -- * The 'View' Monad
        View, ViewT, ViewT',
        runView, runViewT,
        -- * Functions
        doIO, catch
        ) where

import Control.Exception (catchDyn)

import Control.Monad.State
import Control.Monad.Trans (MonadIO(..))
import Data.Maybe
import HSX.XMLGenerator (XMLGenT(..), unXMLGenT)
import qualified Network.HTTP as HTTP
import Prelude hiding (catch)

import Turbinado.Environment
import Turbinado.View.Exception
import Turbinado.Utility.General


--------------------------------------------------------------
-- The View Monad

-- | The View monad is a reader wrapper around
-- the IO monad, but extended with an XMLGenerator wrapper.

type View =  ViewT IO

type ViewT' m = StateT Environment m
type ViewT  m = XMLGenT (ViewT' m)

-- do NOT export this in the final version
dummyEnv = undefined

-- | Runs a View computation in a particular environment. Since View wraps the IO monad,
-- the result of running it will be an IO computation.
runView :: View a -> Environment -> IO (a, Environment)
runView p e = runStateT  (unXMLGenT p) e

runViewT :: ViewT IO a -> Environment -> IO (a, Environment)
runViewT = runStateT . unXMLGenT

-- | Execute an IO computation within the View monad.
doIO :: IO a -> View a
doIO = liftIO

-----------------------------------------------------------------------
-- Exception handling

-- | Catch a user-caused exception.
catch :: View a -> (Exception -> View a) -> View a
catch (XMLGenT (StateT f)) handler = XMLGenT $ StateT $ \e ->
        f e `catchDyn` (\ex -> (let (XMLGenT (StateT g)) = handler ex
                                   in g e))

