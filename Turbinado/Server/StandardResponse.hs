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
import Data.Maybe
import Network.HTTP
import Network.HTTP.Headers
import System.Locale
import System.Time

import Turbinado.Environment.Types
import Turbinado.Environment.Response
import Turbinado.Controller.Monad
import Turbinado.Utility.Data

-- import HSP.Data
instance Eq Header where
  (==) (Header hn1 _) (Header hn2 _) = hn1 == hn2

addEmptyResponse :: (HasEnvironment m) => m ()
addEmptyResponse = 
     do t <- liftIO $ getClockTime
        setResponse (Response (0,0,0) 
                      "" 
                      (startingHeaders t) 
                      ""
                    )

fileNotFoundResponse :: (HasEnvironment m) => FilePath -> m ()
fileNotFoundResponse fp = 
     do t <- liftIO $ getClockTime
        setResponse (Response (4,0,4) 
                              "File Not Found" 
                              (startingHeaders t ++ [Header HdrContentLength $ show $ length body])
                              (body))
            where body = "<html><body>\n <p><big>404 File Not Found</big></p>\n <p>Requested resource: "++ fp ++ "</p>\n </body></html>"

cachedContentResponse :: (HasEnvironment m) => Int -> String -> String -> m ()
cachedContentResponse age ct body =
     do pageResponse [ Header HdrCacheControl $ "max-age=" ++ (show age) ++ ", public"
                     , Header HdrContentType ct]
                     body

pageResponse :: (HasEnvironment m) => [Header] -> String -> m ()
pageResponse hds body =
     do t <- liftIO $ getClockTime
        r <- getEnvironment >>= (return . fromJust' "StandardResponse : pageResponse" . getResponse)
        setResponse $ foldl
                        (\rs (Header hn s) -> replaceHeader hn s rs)
                        (Response 
                          (2,0,0) 
                          "OK"
                          (rspHeaders r ++ [Header HdrContentLength $ show $ length body])
                          body
                        )
                        hds

redirectResponse :: (HasEnvironment m) => String -> m ()
redirectResponse l =
     do t <- liftIO $ getClockTime
        r <- getEnvironment >>= (return . fromJust' "StandardResponse : redirectResponse" . getResponse)
        setResponse (Response (3,0,2) "OK" (rspHeaders r ++ [Header HdrLocation l]) "")

errorResponse :: (HasEnvironment m) => String -> m ()
errorResponse err = 
     do t <- liftIO $ getClockTime
        setResponse (Response (5,0,0) "Internal Server Error"
         	(startingHeaders t  ++ [Header HdrContentLength $ show $ length body]) body)
  where body = "<html><body>\n <p><big>500 Internal Server Error</big></p>\n <p>Error specification:<br/>\n" ++ err ++ "</p>\n </body></html>"

badReqResponse :: (HasEnvironment m) => m ()
badReqResponse =
     do t <- liftIO $ getClockTime
        setResponse (Response (4,0,0) "Bad Request"
        	(startingHeaders t ++ [Header HdrContentLength $ show $ length body]) body)
  where body = "<html><body>\n  <p><big>400 Bad Request</big></p>\n  </body></html>"


startingHeaders t = [ Header HdrServer "Turbinado www.turbinado.org"
                    , Header HdrContentType "text/html; charset=UTF-8"
                    , Header HdrDate $ formatCalendarTime defaultTimeLocale rfc822DateFormat $ toUTCTime t
                    ]

