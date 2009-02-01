module Turbinado.View.Monad (
        -- * The 'View' Monad
        View, ViewT, ViewT',
        runView, runViewT,
        get,
        put,
        -- * Functions
        liftIO, catch
        ) where

import Control.OldException (catchDyn)

import Control.Monad.State
import Control.Monad.Trans (MonadIO(..), liftIO)
import Data.Maybe
import HSX.XMLGenerator (XMLGenT(..), unXMLGenT)
import qualified Network.HTTP as HTTP
import Prelude hiding (catch)

import Turbinado.Environment.Types
import Turbinado.View.Exception
import Turbinado.Utility.General


--------------------------------------------------------------
-- The View Monad

-- | The View monad is a reader wrapper around
-- the IO monad, but extended with an XMLGenerator wrapper.
-- View = XMLGenT (StateT Environment IO) a
type View =  ViewT IO

type ViewT' m = StateT Environment m
type ViewT  m = XMLGenT (ViewT' m)

instance HasEnvironment View where
  getEnvironment = lift get
  setEnvironment = lift . put


getEnvironment :: View Environment
getEnvironment = lift get

setEnvironment :: Environment -> View ()
setEnvironment e = lift $ put e


-- do NOT export this in the final version
dummyEnv = undefined

-- | Runs a View computation in a particular environment. Since View wraps the IO monad,
-- the result of running it will be an IO computation.
runView :: View a -> Environment -> IO (a, Environment)
runView p e = runStateT  (unXMLGenT p) e

runViewT ::  ViewT IO a -> Environment -> IO (a, Environment)
runViewT = runStateT . unXMLGenT

-----------------------------------------------------------------------
-- Exception handling

-- | Catch a user-caused exception.
catch :: View a -> (Exception -> View a) -> View a
catch (XMLGenT (StateT f)) handler = XMLGenT $ StateT $ \e ->
        f e `catchDyn` (\ex -> (let (XMLGenT (StateT g)) = handler ex
                                   in g e))

