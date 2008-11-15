module Turbinado.Environment (
  Environment,
  EnvironmentFilter,
  newEnvironment,
  getKey,
  setKey
  ) where

import Data.Dynamic
import Data.Map
import Data.Maybe
import System.IO
import System.IO.Unsafe
import System.Log.Logger

-- Using Dynamic for two reasons:
--  1) Break module cycles (Environment doesn't import the various Request, Response, etc bits
--  2) Extensibility - easy for plugins to add data to the Environment
type Environment = Map String Dynamic

type EnvironmentFilter = Environment -> IO Environment

newEnvironment :: IO Environment
newEnvironment = return (empty :: Environment)

getKey :: (Typeable a) => String -> Environment -> a
getKey k e =  fromJust $ fromDynamic $ e ! k

setKey :: (Typeable a) => String -> a -> EnvironmentFilter
setKey k v = \e -> return $  insert k (toDyn v) e
