module Turbinado.Server.Network (
          receiveRequest 	-- :: Handle -> IO Request
        , sendResponse		-- :: Handle -> Response -> IO ()
        ) where

import Network.Socket

import Turbinado.Server.Exception
import Turbinado.Environment.Logger
import Turbinado.Environment
import Turbinado.Environment.Request
import Turbinado.Environment.Response

import Network.HTTP


receiveRequest :: Socket -> EnvironmentFilter
receiveRequest sock e = do
        req <-receiveHTTP sock
        case req of
         Left _ -> throwTurbinado $ BadRequest "Looks as though we've got a bad request, sir"
         Right r  -> setRequest r e

sendResponse :: Socket -> Environment -> IO ()
sendResponse sock e = respondHTTP sock $ getResponse e
