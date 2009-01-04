-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.Headers
-- Copyright   :  (c) Warrick Gray 2002, Bjorn Bringert 2003-2005, 2007 Robin Bate Boerop
-- License     :  BSD
-- 
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- * Changes by Robin Bate Boerop <robin@bateboerop.name>:
--      - Made dependencies explicit in import statements.
--      - Removed false dependencies in import statements.
--      - Added missing type signatures.
--      - Created Network.HTTP.Headers from Network.HTTP modules.
--
-- See changes history and TODO list in Network.HTTP module.
--
-- * Header notes:
--
--     [@Host@]
--                  Required by HTTP\/1.1, if not supplied as part
--                  of a request a default Host value is extracted
--                  from the request-uri.
-- 
--     [@Connection@] 
--                  If this header is present in any request or
--                  response, and it's value is "close", then
--                  the current request\/response is the last 
--                  to be allowed on that connection.
-- 
--     [@Expect@]
--                  Should a request contain a body, an Expect
--                  header will be added to the request.  The added
--                  header has the value \"100-continue\".  After
--                  a 417 \"Expectation Failed\" response the request
--                  is attempted again without this added Expect
--                  header.
--                  
--     [@TransferEncoding,ContentLength,...@]
--                  if request is inconsistent with any of these
--                  header values then you may not receive any response
--                  or will generate an error response (probably 4xx).
--
-----------------------------------------------------------------------------
module Network.HTTP.Headers
   ( HasHeaders(..)
   , Header(..)
   , HeaderName(..)
   , insertHeader
   , insertHeaderIfMissing
   , insertHeaders
   , retrieveHeaders
   , replaceHeader
   , findHeader
   , lookupHeader
   , parseHeaders
   , parseHeader
   , headerMap
   ) where

import Data.Char (isSpace, toLower)
import Data.List (elemIndex)
import Network.Stream (Result, ConnError(ErrorParse))

-- remove leading and trailing whitespace.
trim :: String -> String
trim = let dropspace = dropWhile isSpace in
       reverse . dropspace . reverse . dropspace

-- Split a list into two parts, the delimiter occurs
-- at the head of the second list.  Nothing is returned
-- when no occurance of the delimiter is found.
split :: Eq a => a -> [a] -> Maybe ([a],[a])
split delim list = case delim `elemIndex` list of
    Nothing -> Nothing
    Just x  -> Just $ splitAt x list

crlf :: String
crlf = "\r\n"

-- | The Header data type pairs header names & values.
data Header = Header HeaderName String

instance Show Header where
    show (Header key value) = show key ++ ": " ++ value ++ crlf

-- | HTTP Header Name type:
--  Why include this at all?  I have some reasons
--   1) prevent spelling errors of header names,
--   2) remind everyone of what headers are available,
--   3) might speed up searches for specific headers.
--
--  Arguments against:
--   1) makes customising header names laborious
--   2) increases code volume.
--
data HeaderName = 
                 -- Generic Headers --
                  HdrCacheControl
                | HdrConnection
                | HdrDate
                | HdrPragma
                | HdrTransferEncoding        
                | HdrUpgrade                
                | HdrVia

                -- Request Headers --
                | HdrAccept
                | HdrAcceptCharset
                | HdrAcceptEncoding
                | HdrAcceptLanguage
                | HdrAuthorization
                | HdrCookie
                | HdrExpect
                | HdrFrom
                | HdrHost
                | HdrIfModifiedSince
                | HdrIfMatch
                | HdrIfNoneMatch
                | HdrIfRange
                | HdrIfUnmodifiedSince
                | HdrMaxForwards
                | HdrProxyAuthorization
                | HdrRange
                | HdrReferer
                | HdrUserAgent

                -- Response Headers
                | HdrAge
                | HdrLocation
                | HdrProxyAuthenticate
                | HdrPublic
                | HdrRetryAfter
                | HdrServer
                | HdrSetCookie
                | HdrVary
                | HdrWarning
                | HdrWWWAuthenticate

                -- Entity Headers
                | HdrAllow
                | HdrContentBase
                | HdrContentEncoding
                | HdrContentLanguage
                | HdrContentLength
                | HdrContentLocation
                | HdrContentMD5
                | HdrContentRange
                | HdrContentType
                | HdrETag
                | HdrExpires
                | HdrLastModified

                -- Mime entity headers (for sub-parts)
                | HdrContentTransferEncoding

                -- | Allows for unrecognised or experimental headers.
                | HdrCustom String -- not in header map below.
    deriving(Eq)

-- Translation between header names and values,
-- good candidate for improvement.
headerMap :: [ (String,HeaderName) ]
headerMap 
 = [  ("Cache-Control"        ,HdrCacheControl      )
	, ("Connection"           ,HdrConnection        )
	, ("Date"                 ,HdrDate              )    
	, ("Pragma"               ,HdrPragma            )
	, ("Transfer-Encoding"    ,HdrTransferEncoding  )        
	, ("Upgrade"              ,HdrUpgrade           )                
	, ("Via"                  ,HdrVia               )
	, ("Accept"               ,HdrAccept            )
	, ("Accept-Charset"       ,HdrAcceptCharset     )
	, ("Accept-Encoding"      ,HdrAcceptEncoding    )
	, ("Accept-Language"      ,HdrAcceptLanguage    )
	, ("Authorization"        ,HdrAuthorization     )
	, ("From"                 ,HdrFrom              )
	, ("Host"                 ,HdrHost              )
	, ("If-Modified-Since"    ,HdrIfModifiedSince   )
	, ("If-Match"             ,HdrIfMatch           )
	, ("If-None-Match"        ,HdrIfNoneMatch       )
	, ("If-Range"             ,HdrIfRange           ) 
	, ("If-Unmodified-Since"  ,HdrIfUnmodifiedSince )
	, ("Max-Forwards"         ,HdrMaxForwards       )
	, ("Proxy-Authorization"  ,HdrProxyAuthorization)
	, ("Range"                ,HdrRange             )   
	, ("Referer"              ,HdrReferer           )
	, ("User-Agent"           ,HdrUserAgent         )
	, ("Age"                  ,HdrAge               )
	, ("Location"             ,HdrLocation          )
	, ("Proxy-Authenticate"   ,HdrProxyAuthenticate )
	, ("Public"               ,HdrPublic            )
	, ("Retry-After"          ,HdrRetryAfter        )
	, ("Server"               ,HdrServer            )
	, ("Vary"                 ,HdrVary              )
	, ("Warning"              ,HdrWarning           )
	, ("WWW-Authenticate"     ,HdrWWWAuthenticate   )
	, ("Allow"                ,HdrAllow             )
	, ("Content-Base"         ,HdrContentBase       )
	, ("Content-Encoding"     ,HdrContentEncoding   )
	, ("Content-Language"     ,HdrContentLanguage   )
	, ("Content-Length"       ,HdrContentLength     )
	, ("Content-Location"     ,HdrContentLocation   )
	, ("Content-MD5"          ,HdrContentMD5        )
	, ("Content-Range"        ,HdrContentRange      )
	, ("Content-Type"         ,HdrContentType       )
	, ("ETag"                 ,HdrETag              )
	, ("Expires"              ,HdrExpires           )
	, ("Last-Modified"        ,HdrLastModified      )
   	, ("Set-Cookie"           ,HdrSetCookie         )
	, ("Cookie"               ,HdrCookie            )
    , ("Expect"               ,HdrExpect            ) ]


instance Show HeaderName where
    show (HdrCustom s) = s
    show x = case filter ((==x).snd) headerMap of
                [] -> error "headerMap incomplete"
                (h:_) -> fst h

-- | This class allows us to write generic header manipulation functions
-- for both 'Request' and 'Response' data types.
class HasHeaders x where
    getHeaders :: x -> [Header]
    setHeaders :: x -> [Header] -> x

-- Header manipulation functions
insertHeader, replaceHeader, insertHeaderIfMissing
    :: HasHeaders a => HeaderName -> String -> a -> a


-- | Inserts a header with the given name and value.
-- Allows duplicate header names.
insertHeader name value x = setHeaders x newHeaders
    where
        newHeaders = (Header name value) : getHeaders x

-- | Adds the new header only if no previous header shares
-- the same name.
insertHeaderIfMissing name value x = setHeaders x (newHeaders $ getHeaders x)
    where
        newHeaders list@(h@(Header n _): rest)
            | n == name  = list
            | otherwise  = h : newHeaders rest
        newHeaders [] = [Header name value]

-- | Removes old headers with duplicate name.
replaceHeader name value x = setHeaders x newHeaders
    where
        newHeaders = Header name value : [ x | x@(Header n v) <- getHeaders x, name /= n ]
          
-- | Inserts multiple headers.
insertHeaders :: HasHeaders a => [Header] -> a -> a
insertHeaders hdrs x = setHeaders x (getHeaders x ++ hdrs)

-- | Gets a list of headers with a particular 'HeaderName'.
retrieveHeaders :: HasHeaders a => HeaderName -> a -> [Header]
retrieveHeaders name x = filter matchname (getHeaders x)
    where
        matchname (Header n _)  |  n == name  =  True
        matchname _ = False

-- | Lookup presence of specific HeaderName in a list of Headers
-- Returns the value from the first matching header.
findHeader :: HasHeaders a => HeaderName -> a -> Maybe String
findHeader n x = lookupHeader n (getHeaders x)

-- An anomally really:
lookupHeader :: HeaderName -> [Header] -> Maybe String
lookupHeader v (Header n s:t)  |  v == n   =  Just s
                               | otherwise =  lookupHeader v t
lookupHeader _ _  =  Nothing

parseHeader :: String -> Result Header
parseHeader str =
    case split ':' str of
        Nothing -> Left (ErrorParse $ "Unable to parse header: " ++ str)
        Just (k,v) -> Right $ Header (fn k) (trim $ drop 1 v)
    where
        fn k = case map snd $ filter (match k . fst) headerMap of
                 [] -> (HdrCustom k)
                 (h:_) -> h

        match :: String -> String -> Bool
        match s1 s2 = map toLower s1 == map toLower s2
    
parseHeaders :: [String] -> Result [Header]
parseHeaders =
   catRslts [] . map (parseHeader . clean) . joinExtended ""
   where
        -- Joins consecutive lines where the second line
        -- begins with ' ' or '\t'.
        joinExtended old (h : t)
            | not (null h) && (head h == ' ' || head h == '\t')
                = joinExtended (old ++ ' ' : tail h) t
            | otherwise = old : joinExtended h t
        joinExtended old [] = [old]

        clean [] = []
        clean (h:t) | h `elem` "\t\r\n" = ' ' : clean t
                    | otherwise = h : clean t

        -- tollerant of errors?  should parse
        -- errors here be reported or ignored?
        -- currently ignored.
        catRslts :: [a] -> [Result a] -> Result [a]
        catRslts list (h:t) = 
            case h of
                Left _ -> catRslts list t
                Right v -> catRslts (v:list) t
        catRslts list [] = Right $ reverse list            
