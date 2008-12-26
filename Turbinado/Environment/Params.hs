module Turbinado.Environment.Params(
  getParam,
  getParam_u
  ) where

import Data.Maybe
import Network.HTTP
import Network.HTTP.Headers
import Network.URI

import Turbinado.Environment.Header
import Turbinado.Environment.Request
import Turbinado.Environment.Types

getParam_u :: (HasEnvironment m) => String -> m String
getParam_u p =  do r <- getParam p
                   return $ fromJust r

getParam :: (HasEnvironment m) => String -> m (Maybe String)
getParam p = do r <- getParamFromQueryString p
                case r of
                  Just r' -> return r
                  Nothing -> getParamFromBody p
                
getParamFromQueryString :: (HasEnvironment m) => String -> m (Maybe String)
getParamFromQueryString s = do e <- getEnvironment
                               let qs = uriQuery $ rqURI (fromJust $ getRequest e)
                               return $ lookup s $ formDecode qs

getParamFromBody :: (HasEnvironment m) => String -> m (Maybe String)
getParamFromBody s = do e <- getEnvironment
                        ct <- getHeader HdrContentType
                        let rm = rqMethod (fromJust $ getRequest e)
                            rb = rqBody   (fromJust $ getRequest e)
                        case rm of
                          POST -> -- TODO: ADD MULTIPART
                                  return $ lookup s $ formDecode rb
                          _    -> return Nothing

-- LIFTED FROM THE CGI PACKAGE

-- | Gets the name-value pairs from application\/x-www-form-urlencoded data.
formDecode :: String -> [(String,String)]
formDecode "" = []
formDecode s = (urlDecode n, urlDecode (drop 1 v)) : formDecode (drop 1 rs)
        where (nv,rs) = break (=='&') s
              (n,v) = break (=='=') nv

