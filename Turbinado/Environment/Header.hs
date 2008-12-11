module Turbinado.Environment.Header (
  module Turbinado.Environment.Header,
  module Network.HTTP.Headers
  ) where

import Data.Maybe
import Network.HTTP
import Network.HTTP.Headers

import Turbinado.Controller.Monad
import Turbinado.Environment.Types
import Turbinado.Environment.Request

getHeader :: HeaderName -> Controller (Maybe String)
getHeader h = do e <- get
                 return $ findHeader h (fromJust $ getRequest e)


