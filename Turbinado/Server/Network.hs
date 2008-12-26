module Turbinado.Server.Network (
          receiveRequest 	-- :: Handle -> IO Request
        , sendResponse		-- :: Handle -> Response -> IO ()
        ) where

import Data.Maybe
import Network.Socket

import Turbinado.Controller.Monad
import Turbinado.Server.Exception
import Turbinado.Environment.Logger
import Turbinado.Environment.Types
import Turbinado.Environment.Request
import Turbinado.Environment.Response

import Network.HTTP


receiveRequest :: Socket -> Controller ()
receiveRequest sock = do
        req <- liftIO $ receiveHTTP sock
        case req of
         Left _ -> throwTurbinado $ BadRequest "Looks as though we've got a bad request, sir"
         Right r  -> do e <- get
                        put $ e {getRequest = Just r}

sendResponse :: Socket -> Environment -> IO ()
sendResponse sock e = respondHTTP sock $ fromJust $ getResponse e
