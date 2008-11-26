module Turbinado.Environment (
  Environment,
  newEnvironment,
  EnvironmentFilter
  ) where

import Data.Map
import Data.Maybe
import System.IO
import Config.Master

import Turbinado.Environment.CodeStore
import Turbinado.Environment.Logger
import Turbinado.Environment.MimeTypes
import Turbinado.Environment.Request
import Turbinado.Environment.Response
import Turbinado.Environment.Routes
import Turbinado.Environment.Settings
import Turbinado.Environment.ViewData

data Environment = Environment { getCodeStore :: Maybe CodeStore
                               , getLogger    :: Maybe Logger
                               , getMimeTypes :: Maybe MimeTypes
                               , request      :: Maybe Request
                               , getResponse  :: Maybe Response
                               , getRoutes    :: Maybe Routes
                               , getSettings  :: Maybe Settings
                               , getViewData  :: Maybe ViewData
                               , getAppEnvironment :: Maybe AppEnvironment
                               }
                                
type EnvironmentFilter = Environment -> IO Environment

newEnvironment :: IO Environment
newEnvironment = return $ Environment { 
                                 getCodeStore = Nothing
                               , getLogger    = Nothing
                               , getMimeTypes = Nothing
                               , getRequest   = Nothing
                               , getResponse  = Nothing
                               , getRoutes    = Nothing
                               , getSettings  = Nothing
                               , getViewData  = Nothing
                               , getAppEnvironment = Nothing
                               }
                   

