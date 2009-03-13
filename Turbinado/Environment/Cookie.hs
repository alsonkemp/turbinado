-----------------------------------------------------------------------------
-- |
-- Module      :  Turbinado.Environment.Cookie
-- Copyright   :  (c) Alson Kemp 2008-2009
--                (c) Bjorn Bringert 2004-2005
--                (c) Ian Lynagh 2005
-- License     :  BSD-style
--
-- Maintainer  :  alson@alsonkemp.com
-- Stability   :  experimental
-- Portability :  portable
--
--  General server side HTTP cookie library.
--  Based on <http://wp.netscape.com/newsref/std/cookie_spec.html>
--  Lifted in near entirety from the CGI package (http://hackage.haskell.org/cgi-bin/hackage-scripts/package/cgi)
--
-----------------------------------------------------------------------------

module Turbinado.Environment.Cookie 
  ( mkCookie
  , getCookie
  , setCookie
  , deleteCookie
  , showCookie
  ) where

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import Data.Time
import Network.HTTP.Headers

import Turbinado.Environment.Header
import Turbinado.Environment.Response
import Turbinado.Environment.Types
import Turbinado.Utility.Data (fromJust')
--
-- * Getting cookies
--

-- | Get the value of a cookie from a string on the form
--   @\"cookieName1=cookieValue1;...;cookieName2=cookieValue2\"@.
--   This is the format of the @Cookie@ HTTP header.
getCookie :: HasEnvironment m =>
                String -- ^ Cookie name
             -> m (Maybe String)  -- ^ Cookie value, if found
getCookie name   = do e <- getEnvironment
                      h <- getHeader HdrCookie
                      case h of 
                        Nothing -> return Nothing
                        Just h' -> return $ maybeLast [ cv | (cn,cv) <- readCookies h', cn == name ]

-- 
-- * Setting Cookies
--

-- | Set a cookie (which you should have created using something like 'mkCookie'
setCookie :: HasEnvironment m =>
             Cookie ->
             m ()
setCookie c = do e <- getEnvironment
                 let r = fromJust' "setCookie" $ getResponse e
                 setResponse $ insertHeaders [Header HdrSetCookie $ showCookie c] r

-- | Construct a cookie with only name and value set.
--   This client will expire when the browser sessions ends,
--   will only be sent to the server and path which set it
--   and may be sent using any means.
mkCookie :: String -- ^ Name
         -> String -- ^ Value
         -> Cookie -- ^ Cookie
mkCookie name value = Cookie { cookieName = name,
                               cookieValue = value,
                               cookieExpires = Nothing,
                               cookieDomain = Nothing,
                               cookiePath = Nothing
                              }

-- | Delete a cookie from the client by setting the cookie expiry date
--   to a date in the past.
deleteCookie :: HasEnvironment m => 
                String -- ^ Cookie to delete.
                -> m ()
deleteCookie c = setCookie $ c' { cookieExpires = Just epoch }
   where
    c' = mkCookie c ""
    epoch = UTCTime (ModifiedJulianDay 100) (secondsToDiffTime 0)
--
-- * Showing cookies
--

-- | Show a cookie in the format used as the value of the Set-Cookie header.
showCookie :: Cookie -> String
showCookie c = intercalate "; " $
                showPair (cookieName c) (cookieValue c)
                 : catMaybes [expires, path, domain]
    where expires = fmap (showPair "expires" . dateFmt) (cookieExpires c)
          domain = fmap (showPair "domain") (cookieDomain c)
          path = fmap (showPair "path") (maybe (Just "/") Just (cookiePath c))
          dateFmt = formatTime defaultTimeLocale "%a, %d-%b-%Y %H:%M:%S GMT"

--
-- * Reading cookies
--

-- | Gets all the cookies from a Cookie: header value
readCookies :: String             -- ^ String to parse
            -> [(String,String)]  -- ^ Cookie name - cookie value pairs
readCookies s = 
    let (xs,ys) = break (=='=') (dropWhile isSpace s)
        (zs,ws) = break (==';') (dropWhile isSpace (drop 1 ys))
     in if null xs then [] else (xs,zs):readCookies (drop 1 ws)

--
-- * Utilities
--

-- | Return 'Nothing' is the list is empty, otherwise return
--   the last element of the list.
maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just (last xs)

-- | Show a name-value pair. FIXME: if the name or value
--   contains semicolons, this breaks. The problem
--   is that the original cookie spec does not mention
--   how to do escaping or quoting. 
showPair :: String -- ^ name
         -> String -- ^ value
         -> String
showPair name value = name ++ "=" ++ value



