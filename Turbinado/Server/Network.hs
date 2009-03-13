module Turbinado.Server.Network (
          receiveRequest 	-- :: Handle -> IO Request
        , sendResponse		-- :: Handle -> Response -> IO ()
        ) where

import Data.Maybe
import Network.Socket
import Network.HTTP
import Network.URI
import qualified System.Environment as Env
import System.IO

import Turbinado.Controller.Monad
import Turbinado.Server.Exception
import Turbinado.Environment.Logger
import Turbinado.Environment.Types
import Turbinado.Environment.Request
import Turbinado.Environment.Response
import Turbinado.Server.StandardResponse
import Turbinado.Utility.Data



-- | Read the request from client.
receiveRequest :: Maybe Socket -> Controller ()
receiveRequest Nothing     = do e <- getEnvironment
                                acceptCGI
receiveRequest (Just sock) = do
        req <- liftIO $ receiveHTTP sock
        case req of
         Left e -> throwTurbinado $ BadRequest $ "In receiveRequest : " ++ show e
         Right r  -> do e <- get
                        put $ e {getRequest = Just r}

-- | Get the 'Response' from the 'Environment' and send
-- it back to the client.
sendResponse :: Maybe Socket -> Environment -> IO ()
sendResponse Nothing e = respondCGI $ fromJust' "Network : sendResponse" $ getResponse e
sendResponse (Just sock) e = respondHTTP sock $ fromJust' "Network : sendResponse" $ getResponse e

-- | Pull a CGI request from stdin
acceptCGI :: Controller ()
acceptCGI = do body <- liftIO $ hGetContents stdin
               hdrs <- liftIO $ Env.getEnvironment
               let rqheaders = parseHeaders $ extractHTTPHeaders hdrs
                   rquri = fromJust' "Network: acceptCGI: parseURI failed" $ parseURI $ 
                             fromJust' "Network: acceptCGI: No REQUEST_URI in hdrs" $ lookup "SCRIPT_URI" hdrs
                   rqmethod = fromJust' "Network: acceptCGI: REQUEST_METHOD invalid" $ flip lookup rqMethodMap $
                                fromJust' "Network: acceptCGI: No REQUEST_METHOD in hdrs" $ lookup "REQUEST_METHOD" hdrs
               case rqheaders of
                Left err -> errorResponse $ show err
                Right r  -> do e' <- getEnvironment
                               setEnvironment $ e' {
                                getRequest = Just Request { rqURI = rquri
                                                          , rqMethod = rqmethod
                                                          , rqHeaders = r
                                                          , rqBody = body
                                                          }
                                }

matchRqMethod :: String -> RequestMethod
matchRqMethod m = fromJust' "Turbinado.Server.Network:matchRqMethod" $
                    lookup m [ ("GET",    GET)
                             , ("POST",   POST)
                             , ("HEAD",   HEAD)
                             , ("PUT"  ,  PUT)
                             , ("DELETE", DELETE)
                             ]

-- | Convert the HTTP.Response to a CGI response for stdout.
respondCGI :: Response -> IO ()
respondCGI r = do let message = (unlines $ drop 1 $ lines $ show r) ++ "\n\n" ++ rspBody r   -- need to drop the first line from the response for CGI
                  hPutStr stdout message
                  hFlush stdout

-- | Convert from HTTP_SOME_FLAG to Some-Flag for HTTP.parseHeaders
extractHTTPHeaders :: [(String, String)] -> [String]
extractHTTPHeaders [] = []
extractHTTPHeaders (('H':'T':'T':'P':'_':k,v):hs) = (convertUnderscores k ++ ": " ++ v) : extractHTTPHeaders hs
  where convertUnderscores []       = []
        convertUnderscores ('_':ss) = '-' : convertUnderscores ss
        convertUnderscores (s  :ss) =  s  : convertUnderscores ss
extractHTTPHeaders ((k,v) : hs) = extractHTTPHeaders hs


-- | Lifted from Network.HTTP
rqMethodMap :: [(String, RequestMethod)]
rqMethodMap = [("HEAD",    HEAD),
               ("PUT",     PUT),
               ("GET",     GET),
               ("POST",    POST),
               ("DELETE",  DELETE),
               ("OPTIONS", OPTIONS),
               ("TRACE",   TRACE)]


