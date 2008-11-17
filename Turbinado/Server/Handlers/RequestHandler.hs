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
import Turbinado.Environment
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

preFilters :: [EnvironmentFilter]
preFilters = [Routes.runRoutes ]

postFilters :: [EnvironmentFilter]
postFilters = []

requestHandler :: EnvironmentFilter
requestHandler e = do
          debugM e $ " requestHandler : running pre and main filters"
          -- Run the Pre filters, the page
          e' <- foldl ( chainer ) (return e) $ preFilters ++
                                               customPreFilters ++
                                               [ retrieveAndRunController
                                               , retrieveAndRunLayout 
                                               ] 
          debugM e $ " requestHandler : running post filters"
          foldl ( >>= ) (return e') (customPostFilters ++ postFilters)


-- chains EnvironmentFilters together, skipping the
-- remaining filters if the Response is complete
chainer :: IO Environment -> EnvironmentFilter -> IO Environment
chainer m f = do e <- m
                 case isResponseComplete e of
                   True -> return e
                   False -> f e
                   
retrieveAndRunController :: EnvironmentFilter
retrieveAndRunController e =
           do debugM e $ " retrieveAndRunController : Starting"
              debugM e $ " retrieveAndRunController : c = " ++ (show $ (getSetting "controller" e :: Maybe String))
              debugM e $ " retrieveAndRunController : a = " ++ (show $ (getSetting "action" e :: Maybe String))
              let c = fromJust $ getSetting "controller" e -- FIXME: handle the Maybe (!fromJust)
                  a = fromJust $ getSetting "action" e
              debugM e $ " retrieveAndRunController : " ++ c ++ " : " ++ a
              p       <- retrieveCode e CTController (getController e)
              case p of
                 CodeLoadController p' _ _ -> evalController p' e
                 CodeLoadView       _  _ _ -> error "retrieveAndRunView called, but returned CodeLoadView"
                 CodeLoadFailure           -> fileNotFoundResponse c e

retrieveAndRunLayout :: EnvironmentFilter
retrieveAndRunLayout e =
           do let l = getLayout e -- FIXME: handle the Maybe (!fromJust)
              p    <- case l of 
                     ("", _) -> retrieveCode e CTView   (getView e)  -- If no Layout, then pull a View
                     _       -> retrieveCode e CTLayout l
              case p of
                 CodeLoadView       p' _ _ -> evalView p' e
                 CodeLoadController _  _ _ -> error "retrieveAndRunLayout called, but returned CodeLoadController"
                 CodeLoadFailure           -> fileNotFoundResponse (joinPath [(fst l), (snd l)]) e

{-
baseRequestHandler :: HTTP.Request -> CodeStore -> SessionStore -> IO HTTP.Response
baseRequestHandler hreq pages sst = do
        debugM e "Done!"
        debugM e "Generating output ... "
        hds  <- Response.getHeaders resp
        let body = HSP.renderXML xml
        debugM e "Done!"
        where paths hreq = 
                let u = HTTP.rqURI hreq
                    p   =  uriPath u
                    dirp = reverse $ dropWhile (/='/') $ reverse p
                in (rootDir ++ p, rootDir ++ dirp)
-}
