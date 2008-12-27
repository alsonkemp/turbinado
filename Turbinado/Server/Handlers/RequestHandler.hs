-----------------------------------------------------------------------------
-- |
-- Module      :  Turbinado.Server.Handlers.RequestHandler
-- Copyright   :  (c) Alson Kemp 2008, Niklas Broberg 2004,
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  Alson Kemp, Alson@AlsonKemp.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A utility for the server that handles a recieved request, producing
-- a response.
--
-----------------------------------------------------------------------------
module Turbinado.Server.Handlers.RequestHandler (
    requestHandler
    ) where

import qualified Network.HTTP as HTTP
import Network.URI
import Prelude hiding (catch)
import System.Directory
import System.FilePath
import System.Plugins
import Control.Monad
import Data.Maybe
import Data.List
import Data.Dynamic

import Config.Master
import Turbinado.Environment.Types
import Turbinado.Environment.CodeStore
import Turbinado.Environment.Logger
import Turbinado.Environment.Request
import qualified Turbinado.Environment.Routes as Routes
import Turbinado.Environment.Settings
import Turbinado.Server.Exception
import Turbinado.Controller
import Turbinado.View
import Turbinado.View.XML
import Turbinado.Server.StandardResponse

preFilters :: [Controller ()]
preFilters = [Routes.runRoutes ]

postFilters :: [Controller ()]
postFilters = []

requestHandler :: Controller ()
requestHandler = do
          debugM $ " requestHandler : running pre and main filters"
          -- Run the Pre filters, the page
          sequence_ $ preFilters ++
                      customPreFilters ++
                      [ retrieveAndRunController
                      , retrieveAndRunLayout
                      ] 
          debugM $ " requestHandler : running post filters"
          sequence_ (customPostFilters ++ postFilters)


retrieveAndRunController :: Controller ()
retrieveAndRunController =
           do debugM $ " retrieveAndRunController : Starting"
              co <- getController
              p  <- retrieveCode CTController co
              case p of
                 CodeLoadController p' _ _ -> p'
                 CodeLoadFailure    e      -> errorResponse e
                 CodeLoadView       _  _ _ -> error "retrieveAndRunController: retrieveCode called, but returned CodeLoadView"
                 CodeLoadMissing           -> error "retrieveAndRunController: retrieveCode called, but returned CodeLoadMissing"

retrieveAndRunLayout :: Controller ()
retrieveAndRunLayout =
           do e <- getEnvironment
              case (isResponseComplete e) of
                True -> return ()
                False -> do l <- getLayout
                            p    <- case l of 
                                     ("", _) -> do v <- getView
                                                   retrieveCode CTView v    -- If no Layout, then pull a View
                                     _       ->    retrieveCode CTLayout l
                            case p of
                              CodeLoadView       p' _ _ -> evalView p'
                              CodeLoadFailure    e      -> errorResponse e
                              CodeLoadController _  _ _ -> error "retrieveAndRunView: retrieveCode called, but returned CodeLoadController"
                              CodeLoadMissing           -> error "retrieveAndRunView: retrieveCode called, but returned CodeLoadMissing"


