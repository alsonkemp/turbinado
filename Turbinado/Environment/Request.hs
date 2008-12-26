module Turbinado.Environment.Request (
        HTTP.Request(..),
        addRequestToEnvironment,
        ) where

import qualified Network.HTTP as HTTP
import Network.URI
import Turbinado.Utility.General
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Turbinado.Environment.Types

addRequestToEnvironment :: (HasEnvironment m) => HTTP.Request -> m ()
addRequestToEnvironment req = do e <- getEnvironment
                                 setEnvironment $ e {getRequest = Just $ req}


