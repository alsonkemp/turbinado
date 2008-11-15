-----------------------------------------------------------------------------
-- |
-- Module      :  Turbinado.StandardResponse
-- Copyright   :  (c) Andreas Farre, Niklas Broberg, 2004
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Andreas Farre, d00farre@dtek.chalmers.se,
--        	  Niklas Broberg, d00nibro@dtek.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- A set of functions to create standard HTTP responses.
-----------------------------------------------------------------------------
module Turbinado.Server.StandardResponse where

import Network.HTTP
import Network.HTTP.Headers
import Turbinado.Environment
import Turbinado.Environment.Response

-- import HSP.Data

fileNotFoundResponse :: FilePath -> EnvironmentFilter
fileNotFoundResponse fp =
        setResponse (Response (4,0,0) "File Not Found" (contentHds $ length body) (body))
  where body = "<html><body>\n <p><big>404 File Not Found</big></p>\n <p>Requested resource: "++ fp ++ "</p>\n </body></html>"

cachedContentResponse :: Int -> String -> String -> EnvironmentFilter
cachedContentResponse age ct body =
      pageResponse [ Header HdrCacheControl $ "max-age=" ++ (show age) ++ ", public"
                   , Header HdrContentType ct] body

pageResponse :: [Header] -> String -> EnvironmentFilter
pageResponse hds body =
        setResponse (Response stSuccess "OK" 
        	(contentHds (length body) ++ hds) (body))

errorResponse :: String -> EnvironmentFilter
errorResponse err = 
        setResponse (Response stError "Internal Server Error"
         	(contentHds $ length body) (body))
  where body = "<html><body>\n <p><big>500 Internal Server Error</big></p>\n <p>Error specification:<br/>\n" ++ err ++ "</p>\n </body></html>"

badReqResponse :: EnvironmentFilter
badReqResponse =
        setResponse (Response stBadReq "Bad Request"
        	(contentHds $ length body) (body))
  where body = "<html><body>\n  <p><big>400 Bad Request</big></p>\n  </body></html>"


contentHds :: Int -> [Header]
contentHds l = [Header HdrContentType"text/html",
                Header HdrContentLength $ show l]

stSuccess, stFNF :: ResponseCode
stSuccess = (2,0,0)
stFNF = (4,0,4)
stError = (5,0,0)
stBadReq = (4,0,0)

