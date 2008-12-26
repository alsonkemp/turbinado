module Turbinado.Server.StaticContent (
    tryStaticContent
    ) where

import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.FilePath
import System.IO
import System.Directory
import Prelude hiding (catch)

import Network.URI
import Network.HTTP
import Turbinado.Controller.Monad
import Turbinado.Server.StandardResponse
import Turbinado.Environment.MimeTypes
import Turbinado.Environment.Types
import Turbinado.Environment.Logger
import Turbinado.Environment.Request
import Turbinado.Environment.Response

import Config.Master

tryStaticContent :: Controller ()
tryStaticContent = 
  do e <- get
     cDir <- liftIO $ getCurrentDirectory
     let mt      = fromJust $ getMimeTypes e
         rq      = fromJust $ getRequest e
         f       = drop 1 $ uriPath $ rqURI rq
         trydirs = case (length f) of
           0 -> map (\s -> joinPath $ map normalise [cDir, s, "index.html"]) staticDirs
           _ -> map (\s -> joinPath $ map normalise [cDir, s, f]) staticDirs
     -- debugM e $ "  tryStaticContent over " ++ (show trydirs)
     sequence_ $ map (tryToGetStaticContent mt) trydirs

tryToGetStaticContent :: MimeTypes -> FilePath -> Controller ()
tryToGetStaticContent mt p = do exist <- liftIO $ doesFileExist p
                                case exist of
                                    False -> return ()
                                    True -> do f <- liftIO $ readFile p
                                               let ct = maybe "text/html" (show) (mimeTypeOf mt p)
                                               cachedContentResponse 600 ct f
