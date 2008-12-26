-----------------------------------------------------------------------------
-- |
-- Module      :  Turbinado.Server.StandardResponse
-- Copyright   :  (c) Alson Kemp 2008, Andreas Farre, Niklas Broberg, 2004
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  Alson Kemp (Alson@AlsonKemp.com)
-- Stability   :  experimental
-- Portability :  portable
--
-- A set of functions to create standard HTTP responses.
-----------------------------------------------------------------------------
module Turbinado.Server.StandardResponse where

import Data.List
import Network.HTTP
import Network.HTTP.Headers
import System.Locale
import System.Time

import Turbinado.Environment.Types
import Turbinado.Environment.Response
import Turbinado.Controller.Monad

-- import HSP.Data
instance Eq Header where
  (==) (Header hn1 _) (Header hn2 _) = hn1 == hn2

fileNotFoundResponse :: (HasEnvironment m) => FilePath -> m ()
fileNotFoundResponse fp = 
     do t <- liftIO $ getClockTime
        setResponse (Response (4,0,0) 
                     "File Not Found" 
                     (buildHeaders (Just $ length body) t []) 
                     (body))
  where body = "<html><body>\n <p><big>404 File Not Found</big></p>\n <p>Requested resource: "++ fp ++ "</p>\n </body></html>"

cachedContentResponse :: (HasEnvironment m) => Int -> String -> String -> m ()
cachedContentResponse age ct body =
     do t <- liftIO $ getClockTime
        pageResponse (buildHeaders 
                        Nothing t
                        [Header HdrCacheControl $ "max-age=" ++ (show age) ++ ", public"
                        , Header HdrContentType ct])
                     body

pageResponse :: (HasEnvironment m) => [Header] -> String -> m ()
pageResponse hds body =
     do t <- liftIO $ getClockTime
        setResponse (Response stSuccess "OK" 
        	(buildHeaders (Just $ length body) t hds) (body))

redirectResponse :: (HasEnvironment m) => String -> m ()
redirectResponse l =
     do t <- liftIO $ getClockTime
        setResponse (Response (3,0,2) "OK" (buildHeaders Nothing t [Header HdrLocation l]) "")

errorResponse :: (HasEnvironment m) => String -> m ()
errorResponse err = 
     do t <- liftIO $ getClockTime
        setResponse (Response stError "Internal Server Error"
         	(buildHeaders (Just $ length body) t []) (body))
  where body = "<html><body>\n <p><big>500 Internal Server Error</big></p>\n <p>Error specification:<br/>\n" ++ err ++ "</p>\n </body></html>"

badReqResponse :: (HasEnvironment m) => m ()
badReqResponse =
     do t <- liftIO $ getClockTime
        setResponse (Response stBadReq "Bad Request"
        	(buildHeaders (Just $ length body) t []) body)
  where body = "<html><body>\n  <p><big>400 Bad Request</big></p>\n  </body></html>"


buildHeaders :: Maybe Int -> ClockTime -> [Header] -> [Header]
buildHeaders Nothing  t hdrs = union hdrs ( startingHeaders t)
buildHeaders (Just l) t hdrs = union hdrs ((startingHeaders t) ++
                                [Header HdrContentLength $ show l]) 
                                

startingHeaders t = [ Header HdrServer "Turbinado www.turbinado.org"
                    , Header HdrContentType "text/html; charset=UTF-8"
                    , Header HdrDate $ formatCalendarTime defaultTimeLocale rfc822DateFormat $ toUTCTime t
                    ]

stSuccess, stFNF :: ResponseCode
stSuccess = (2,0,0)
stFNF = (4,0,4)
stError = (5,0,0)
stBadReq = (4,0,0)

