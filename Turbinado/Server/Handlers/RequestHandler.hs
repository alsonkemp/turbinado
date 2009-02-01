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

-- | Filters to be run before the Controller are run
-- (e.g. a request logger or session initializer).  Filters
-- should consider what to do if isResponseComplete.  For example,
-- the Controller and View won't run if the response is complete
-- because they would risk overwriting the response.
--
-- NOTE: Custom filters are specified in Config/App.hs.
preFilters :: [Controller ()]
preFilters = [Routes.runRoutes ]

-- | Filters to be run after the Controller and View are run.
-- (e.g. cookie setter)
--
-- NOTE: Custom filters are specified in Config/App.hs.
postFilters :: [Controller ()]
postFilters = []

-- | The main request handler.  This runs standard and custom preFilters
-- then runs the Controller and View.
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


-- | This function dynamically loads (if needed) the 'Controller'
-- using the information provided by the 'Routes'.  Controllers reside
-- in @App/Controllers@.
retrieveAndRunController :: Controller ()
retrieveAndRunController =
           do debugM $ " retrieveAndRunController : Starting"
              e <- getEnvironment
              case (isResponseComplete e) of
                True -> do debugM $ " retrieveAndRunController : response was already complete"
                           return ()
                False -> do co <- getController
                            p  <- retrieveCode CTController co
                            case p of
                              CodeLoadController p' _ -> p'
                              CodeLoadFailure    e    -> errorResponse e
                              CodeLoadView       _  _ -> error "retrieveAndRunController: retrieveCode called, but returned CodeLoadView"
                              CodeLoadMissing         -> error "retrieveAndRunController: retrieveCode called, but returned CodeLoadMissing"

-- | This function dynamically loads (if needed) the 'View'
-- using the information provided by the 'Routes'.  Views reside
-- in @App/Views@ and Layouts reside in @App/Layouts.  
-- The 'View' must contain a @markup@ function.
-- The first 'View' loaded is usually the Layout, which itself
-- loads the actual 'View'.  If the @layout@ setting is empty, then
-- no layout is loaded and the default 'View' is loaded.
retrieveAndRunLayout :: Controller ()
retrieveAndRunLayout =
           do e <- getEnvironment
              case (isResponseComplete e) of
                True -> do debugM $ " retrieveAndRunLayout : response was already complete"
                           return ()
                False -> do l <- getSetting "layout"
                            p    <- case l of 
                                     Nothing -> do v <- getView
                                                   retrieveCode CTView v    -- If no Layout, then pull a View
                                     Just l' ->    retrieveCode CTLayout (l', "markup")
                            case p of
                              CodeLoadView       p' _ -> evalView p'
                              CodeLoadFailure    e    -> errorResponse e
                              CodeLoadController _  _ -> error "retrieveAndRunLayout: retrieveCode called, but returned CodeLoadController"
                              CodeLoadMissing         -> error "retrieveAndRunLayout: retrieveCode called, but returned CodeLoadMissing"


