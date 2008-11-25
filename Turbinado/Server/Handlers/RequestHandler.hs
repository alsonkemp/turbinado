-----------------------------------------------------------------------------
-- |
-- Module      :  Turbinado.RequestHandler
-- Copyright   :  (c) Niklas Broberg 2004,
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Niklas Broberg, d00nibro@dtek.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- A utility for the server that handles a recieved request, producing
-- a response.
--
-- TODO: * Session variable handling
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

requestHandler :: Environment -> IO Environment
requestHandler e = runController requestHandler' e

requestHandler' :: Controller ()
requestHandler' = do
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
              c <- getSetting_u "controller"
              a <- getSetting_u "action"
              debugM $ " retrieveAndRunController : " ++ c ++ " : " ++ a
              co <- getController
              p  <- retrieveCode CTController co
              case p of
                 CodeLoadController p' _ _ -> p'
                 CodeLoadView       _  _ _ -> error "retrieveAndRunView called, but returned CodeLoadView"
                 CodeLoadFailure           -> fileNotFoundResponse c

retrieveAndRunLayout :: Controller ()
retrieveAndRunLayout =
           do l <- getLayout
              p    <- case l of 
                     ("", _) -> do v <- getView
                                   retrieveCode CTView v    -- If no Layout, then pull a View
                     _       -> retrieveCode CTLayout l
              case p of
                 CodeLoadView       p' _ _ -> evalView p'
                 CodeLoadController _  _ _ -> error "retrieveAndRunLayout called, but returned CodeLoadController"
                 CodeLoadFailure           -> fileNotFoundResponse (joinPath [(fst l), (snd l)])


