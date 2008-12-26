module Turbinado.Environment.Response (
        HTTP.Response,
        setResponse,
        isResponseComplete
        )where

import qualified Network.HTTP as HTTP
import Network.URI
import Turbinado.Utility.General
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Turbinado.Environment.Types
import System.Time
import System.Locale


setResponse :: (HasEnvironment m) => HTTP.Response -> m ()
setResponse resp = do e <- getEnvironment
                      setEnvironment $ e {getResponse = Just resp}

isResponseComplete :: Environment -> Bool
isResponseComplete e =  case (getResponse e) of
                          Nothing -> False
                          Just r' -> (HTTP.rspCode r' /= (0,0,0))

