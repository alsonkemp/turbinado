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


-- | Read the request from client.
receiveRequest :: Socket -> Controller ()
receiveRequest sock = do
        req <- liftIO $ receiveHTTP sock
        case req of
         Left e -> throwTurbinado $ BadRequest $ "In receiveRequest : " ++ show e
         Right r  -> do e <- get
                        put $ e {getRequest = Just r}

-- | Get the 'Response' from the 'Environment' and send
-- it back to the client.
sendResponse :: Socket -> Environment -> IO ()
sendResponse sock e = respondHTTP sock $ fromJust $ getResponse e
