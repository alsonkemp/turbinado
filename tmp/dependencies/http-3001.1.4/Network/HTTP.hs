-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP
-- Copyright   :  (c) Warrick Gray 2002, Bjorn Bringert 2003-2005, 2007 Robin Bate Boerop
-- License     :  BSD
-- 
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- An easy HTTP interface enjoy.
--
-- * Changes by Robin Bate Boerop <robin@bateboerop.name>:
--      - Made dependencies explicit in import statements.
--      - Removed false dependencies in import statements.
--      - Added missing type signatures.
--      - Moved Header-related code to Network.HTTP.Headers module.
--
-- * Changes by Simon Foster:
--      - Split module up into to sepearate Network.[Stream,TCP,HTTP] modules
--      - Created functions receiveHTTP and responseHTTP to allow server side interactions
--        (although 100-continue is unsupported and I haven't checked for standard compliancy).
--      - Pulled the transfer functions from sendHTTP to global scope to allow access by
--        above functions.
--
-- * Changes by Graham Klyne:
--      - export httpVersion
--      - use new URI module (similar to old, but uses revised URI datatype)
--
-- * Changes by Bjorn Bringert:
--
--      - handle URIs with a port number
--      - added debugging toggle
--      - disabled 100-continue transfers to get HTTP\/1.0 compatibility
--      - change 'ioError' to 'throw'
--      - Added simpleHTTP_, which takes a stream argument.
--
-- * Changes from 0.1
--      - change 'openHTTP' to 'openTCP', removed 'closeTCP' - use 'close' from 'Stream' class.
--      - added use of inet_addr to openHTTP, allowing use of IP "dot" notation addresses.
--      - reworking of the use of Stream, including alterations to make 'sendHTTP' generic
--        and the addition of a debugging stream.
--      - simplified error handling.
-- 
-- * TODO
--     - request pipelining
--     - https upgrade (includes full TLS, i.e. SSL, implementation)
--         - use of Stream classes will pay off
--         - consider C implementation of encryption\/decryption
--     - comm timeouts
--     - MIME & entity stuff (happening in separate module)
--     - support \"*\" uri-request-string for OPTIONS request method
-- 
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
--
-- * Response code notes
-- Some response codes induce special behaviour:
--
--   [@1xx@]   \"100 Continue\" will cause any unsent request body to be sent.
--             \"101 Upgrade\" will be returned.
--             Other 1xx responses are ignored.
-- 
--   [@417@]   The reason for this code is \"Expectation failed\", indicating
--             that the server did not like the Expect \"100-continue\" header
--             added to a request.  Receipt of 417 will induce another
--             request attempt (without Expect header), unless no Expect header
--             had been added (in which case 417 response is returned).
--
-----------------------------------------------------------------------------
module Network.HTTP (
    module Network.Stream,
    module Network.TCP,

    -- ** Constants
    httpVersion,
    
    -- ** HTTP 
    Request(..),
    RequestData,
    Response(..),
    RequestMethod(..),
    ResponseCode,
    simpleHTTP, simpleHTTP_,
    sendHTTP,
    receiveHTTP,
    processRequest,
    getRequestHead,
    respondHTTP,

    -- ** Header Functions
    module Network.HTTP.Headers,

    -- ** URL Encoding
    urlEncode,
    urlDecode,
    urlEncodeVars,

    -- ** URI authority parsing
    URIAuthority(..),
    getAuth,
    parseURIAuthority
) where


-----------------------------------------------------------------
------------------ Imports --------------------------------------
-----------------------------------------------------------------

import Network.URI
   ( URI(URI, uriScheme, uriAuthority, uriPath)
   , URIAuth(uriUserInfo, uriRegName, uriPort)
   , parseURIReference
   , unEscapeString, escapeURIString, isUnescapedInURI
   )
import Network.HTTP.Headers
import Network.Stream
import Network.StreamDebugger (debugStream)
import Network.TCP (openTCPPort)

import Control.Exception as Exception (catch, throw)
import Data.Bits ((.&.))
import Data.Char (isSpace, intToDigit, digitToInt, ord, chr, toLower)
import Data.List (partition, intersperse)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Monad (when, guard)
import Numeric (readHex)
import Text.Read.Lex (readDecP)
import Text.ParserCombinators.ReadP
   ( ReadP, readP_to_S, char, (<++), look, munch )
import Data.Typeable

-- Turn on to enable HTTP traffic logging
debug :: Bool
debug = False

-- File that HTTP traffic logs go to
httpLogFile :: String
httpLogFile = "http-debug.log"

-----------------------------------------------------------------
------------------ Misc -----------------------------------------
-----------------------------------------------------------------

-- remove leading and trailing whitespace.
trim :: String -> String
trim = let dropspace = dropWhile isSpace in
       reverse . dropspace . reverse . dropspace


crlf, sp :: String
crlf = "\r\n"
sp   = " "

-----------------------------------------------------------------
------------------ URI Authority parsing ------------------------
-----------------------------------------------------------------

data URIAuthority = URIAuthority { user :: Maybe String, 
				   password :: Maybe String,
				   host :: String,
				   port :: Maybe Int
				 } deriving (Eq,Show)

-- | Parse the authority part of a URL.
--
-- > RFC 1738, section 3.1:
-- >
-- >       //<user>:<password>@<host>:<port>/<url-path>
-- >  Some or all of the parts "<user>:<password>@", ":<password>",
-- >  ":<port>", and "/<url-path>" may be excluded.
parseURIAuthority :: String -> Maybe URIAuthority
parseURIAuthority s = listToMaybe (map fst (readP_to_S pURIAuthority s))


pURIAuthority :: ReadP URIAuthority
pURIAuthority = do
		(u,pw) <- (pUserInfo `before` char '@') 
			  <++ return (Nothing, Nothing)
		h <- munch (/=':')
		p <- orNothing (char ':' >> readDecP)
		look >>= guard . null 
		return URIAuthority{ user=u, password=pw, host=h, port=p }

pUserInfo :: ReadP (Maybe String, Maybe String)
pUserInfo = do
	    u <- orNothing (munch (`notElem` ":@"))
	    p <- orNothing (char ':' >> munch (/='@'))
	    return (u,p)

before :: Monad m => m a -> m b -> m a
before a b = a >>= \x -> b >> return x

orNothing :: ReadP a -> ReadP (Maybe a)
orNothing p = fmap Just p <++ return Nothing

-----------------------------------------------------------------
------------------ HTTP Messages --------------------------------
-----------------------------------------------------------------


-- Protocol version
httpVersion :: String
httpVersion = "HTTP/1.1"


-- | The HTTP request method, to be used in the 'Request' object.
-- We are missing a few of the stranger methods, but these are
-- not really necessary until we add full TLS.
data RequestMethod = HEAD | PUT | GET | POST | DELETE | OPTIONS | TRACE | Custom String
    deriving(Show,Eq)

rqMethodMap :: [(String, RequestMethod)]
rqMethodMap = [("HEAD",    HEAD),
	       ("PUT",     PUT),
	       ("GET",     GET),
	       ("POST",    POST),
               ("DELETE",  DELETE),
	       ("OPTIONS", OPTIONS),
	       ("TRACE",   TRACE)]

-- | An HTTP Request.
-- The 'Show' instance of this type is used for message serialisation,
-- which means no body data is output.
data Request =
     Request { rqURI       :: URI   -- ^ might need changing in future
                                    --  1) to support '*' uri in OPTIONS request
                                    --  2) transparent support for both relative
                                    --     & absolute uris, although this should
                                    --     already work (leave scheme & host parts empty).
             , rqMethod    :: RequestMethod             
             , rqHeaders   :: [Header]
             , rqBody      :: String
             } deriving (Typeable)



-- Notice that request body is not included,
-- this show function is used to serialise
-- a request for the transport link, we send
-- the body separately where possible.
instance Show Request where
    show (Request u m h _) =
        show m ++ sp ++ alt_uri ++ sp ++ httpVersion ++ crlf
        ++ foldr (++) [] (map show h) ++ crlf
        where
            alt_uri = show $ if null (uriPath u) || head (uriPath u) /= '/' 
                        then u { uriPath = '/' : uriPath u } 
                        else u

instance HasHeaders Request where
    getHeaders = rqHeaders
    setHeaders rq hdrs = rq { rqHeaders=hdrs }

type ResponseCode  = (Int,Int,Int)
type ResponseData  = (ResponseCode,String,[Header])
type RequestData   = (RequestMethod,URI,[Header])

-- | An HTTP Response.
-- The 'Show' instance of this type is used for message serialisation,
-- which means no body data is output, additionally the output will
-- show an HTTP version of 1.1 instead of the actual version returned
-- by a server.
data Response =
    Response { rspCode     :: ResponseCode
             , rspReason   :: String
             , rspHeaders  :: [Header]
             , rspBody     :: String
             } deriving (Typeable)
                   
-- This is an invalid representation of a received response, 
-- since we have made the assumption that all responses are HTTP/1.1
instance Show Response where
    show (Response (a,b,c) reason headers _) =
        httpVersion ++ ' ' : map intToDigit [a,b,c] ++ ' ' : reason ++ crlf
        ++ foldr (++) [] (map show headers) ++ crlf

instance HasHeaders Response where
    getHeaders = rspHeaders
    setHeaders rsp hdrs = rsp { rspHeaders=hdrs }

-----------------------------------------------------------------
------------------ Parsing --------------------------------------
-----------------------------------------------------------------

-- Parsing a request
parseRequestHead :: [String] -> Result RequestData
parseRequestHead [] = Left ErrorClosed
parseRequestHead (com:hdrs) =
    requestCommand com `bindE` \(version,rqm,uri) ->
    parseHeaders hdrs `bindE` \hdrs' ->
    Right (rqm,uri,hdrs')
    where
        requestCommand line
	    =  case words line of
                yes@(rqm:uri:version) -> case (parseURIReference uri, lookup rqm rqMethodMap) of
					  (Just u, Just r) -> Right (version,r,u)
					  _                -> Left (ErrorParse $ "Request command line parse failure: " ++ line)
		no -> if null line
			       then Left ErrorClosed
			       else Left (ErrorParse $ "Request command line parse failure: " ++ line)  

-- Parsing a response
parseResponseHead :: [String] -> Result ResponseData
parseResponseHead [] = Left ErrorClosed
parseResponseHead (sts:hdrs) = 
    responseStatus sts `bindE` \(version,code,reason) ->
    parseHeaders hdrs `bindE` \hdrs' ->
    Right (code,reason,hdrs')
    where

        responseStatus line
            =  case words line of
                yes@(version:code:reason) -> Right (version,match code,concatMap (++" ") reason)
                no -> if null line 
                    then Left ErrorClosed  -- an assumption
                    else Left (ErrorParse $ "Response status line parse failure: " ++ line)


        match [a,b,c] = (digitToInt a,
                         digitToInt b,
                         digitToInt c)
        match _ = (-1,-1,-1)  -- will create appropriate behaviour


        

-----------------------------------------------------------------
------------------ HTTP Send / Recv ----------------------------------
-----------------------------------------------------------------

data Behaviour = Continue
               | Retry
               | Done
               | ExpectEntity
               | DieHorribly String

matchResponse :: RequestMethod -> ResponseCode -> Behaviour
matchResponse rqst rsp =
    case rsp of
        (1,0,0) -> Continue
        (1,0,1) -> Done        -- upgrade to TLS
        (1,_,_) -> Continue    -- default
        (2,0,4) -> Done
        (2,0,5) -> Done
        (2,_,_) -> ans
        (3,0,4) -> Done
        (3,0,5) -> Done
        (3,_,_) -> ans
        (4,1,7) -> Retry       -- Expectation failed
        (4,_,_) -> ans
        (5,_,_) -> ans
        (a,b,c) -> DieHorribly ("Response code " ++ map intToDigit [a,b,c] ++ " not recognised")
    where
        ans | rqst == HEAD = Done
            | otherwise    = ExpectEntity
        

-- | Simple way to get a resource across a non-persistant connection.
-- Headers that may be altered:
--  Host        Altered only if no Host header is supplied, HTTP\/1.1
--              requires a Host header.
--  Connection  Where no allowance is made for persistant connections
--              the Connection header will be set to "close"
simpleHTTP :: Request -> IO (Result Response)
simpleHTTP r = 
    do 
       auth <- getAuth r
       c <- openTCPPort (host auth) (fromMaybe 80 (port auth))
       simpleHTTP_ c r

-- | Like 'simpleHTTP', but acting on an already opened stream.
simpleHTTP_ :: Stream s => s -> Request -> IO (Result Response)
simpleHTTP_ s r =
    do 
       auth <- getAuth r
       let r' = fixReq auth r 
       rsp <- if debug then do
	        s' <- debugStream httpLogFile s
	        sendHTTP s' r'
	       else
	        sendHTTP s r'
       -- already done by sendHTTP because of "Connection: close" header
       --; close s 
       return rsp
       where
  {- RFC 2616, section 5.1.2:
     "The most common form of Request-URI is that used to identify a
      resource on an origin server or gateway. In this case the absolute
      path of the URI MUST be transmitted (see section 3.2.1, abs_path) as
      the Request-URI, and the network location of the URI (authority) MUST
      be transmitted in a Host header field." -}
  -- we assume that this is the case, so we take the host name from
  -- the Host header if there is one, otherwise from the request-URI.
  -- Then we make the request-URI an abs_path and make sure that there
  -- is a Host header.
             fixReq :: URIAuthority -> Request -> Request
	     fixReq URIAuthority{host=h,port=p} r = 
		 let h' = h ++ maybe "" ((':':) . show) p in
		 replaceHeader HdrConnection "close" $
		 insertHeaderIfMissing HdrHost h' $
		 r { rqURI = (rqURI r){ uriScheme = "", 
					uriAuthority = Nothing } }	       

getAuth :: Monad m => Request -> m URIAuthority
getAuth r = case parseURIAuthority auth of
			 Just x -> return x 
			 Nothing -> fail $ "Error parsing URI authority '"
				           ++ auth ++ "'"
		 where auth = case findHeader HdrHost r of
			      Just h -> h
			      Nothing -> uriToAuthorityString (rqURI r)

sendHTTP :: Stream s => s -> Request -> IO (Result Response)
sendHTTP conn rq = 
    do { let a_rq = fixHostHeader rq
       ; rsp <- Exception.catch (main a_rq)
                      (\e -> do { close conn; throw e })
       ; let fn list = when (or $ map findConnClose list)
                            (close conn)
       ; either (\_ -> fn [rqHeaders rq])
                (\r -> fn [rqHeaders rq,rspHeaders r])
                rsp
       ; return rsp
       }
    where       
-- From RFC 2616, section 8.2.3:
-- 'Because of the presence of older implementations, the protocol allows
-- ambiguous situations in which a client may send "Expect: 100-
-- continue" without receiving either a 417 (Expectation Failed) status
-- or a 100 (Continue) status. Therefore, when a client sends this
-- header field to an origin server (possibly via a proxy) from which it
-- has never seen a 100 (Continue) status, the client SHOULD NOT wait
-- for an indefinite period before sending the request body.'
--
-- Since we would wait forever, I have disabled use of 100-continue for now.
        main :: Request -> IO (Result Response)
        main rqst =
            do 
	       --let str = if null (rqBody rqst)
               --              then show rqst
               --              else show (insertHeader HdrExpect "100-continue" rqst)
               writeBlock conn (show rqst)
	       -- write body immediately, don't wait for 100 CONTINUE
	       writeBlock conn (rqBody rqst)
               rsp <- getResponseHead               
               switchResponse True False rsp rqst
        
        -- reads and parses headers
        getResponseHead :: IO (Result ResponseData)
        getResponseHead =
            do { lor <- readTillEmpty1 conn
               ; return $ lor `bindE` parseResponseHead
               }

        -- Hmmm, this could go bad if we keep getting "100 Continue"
        -- responses...  Except this should never happen according
        -- to the RFC.
        switchResponse :: Bool {- allow retry? -}
                       -> Bool {- is body sent? -}
                       -> Result ResponseData
                       -> Request
                       -> IO (Result Response)
            
        switchResponse _ _ (Left e) _ = return (Left e)
                -- retry on connreset?
                -- if we attempt to use the same socket then there is an excellent
                -- chance that the socket is not in a completely closed state.

        switchResponse allow_retry bdy_sent (Right (cd,rn,hdrs)) rqst =
            case matchResponse (rqMethod rqst) cd of
                Continue
                    | not bdy_sent -> {- Time to send the body -}
                        do { val <- writeBlock conn (rqBody rqst)
                           ; case val of
                                Left e -> return (Left e)
                                Right _ ->
                                    do { rsp <- getResponseHead
                                       ; switchResponse allow_retry True rsp rqst
                                       }
                           }
                    | otherwise -> {- keep waiting -}
                        do { rsp <- getResponseHead
                           ; switchResponse allow_retry bdy_sent rsp rqst                           
                           }

                Retry -> {- Request with "Expect" header failed.
                                Trouble is the request contains Expects
                                other than "100-Continue" -}
                    do { writeBlock conn (show rqst ++ rqBody rqst)
                       ; rsp <- getResponseHead
                       ; switchResponse False bdy_sent rsp rqst
                       }   
                     
                Done ->
                    return (Right $ Response cd rn hdrs "")

                DieHorribly str ->
                    return $ Left $ ErrorParse ("Invalid response: " ++ str)

                ExpectEntity ->
                    let tc = lookupHeader HdrTransferEncoding hdrs
                        cl = lookupHeader HdrContentLength hdrs
                    in
                    do { rslt <- case tc of
                          Nothing -> 
                              case cl of
                                  Just x  -> linearTransfer conn (read x :: Int)
                                  Nothing -> hopefulTransfer conn ""
                          Just x  -> 
                              case map toLower (trim x) of
                                  "chunked" -> chunkedTransfer conn
                                  _         -> uglyDeathTransfer conn
                       ; return $ rslt `bindE` \(ftrs,bdy) -> Right (Response cd rn (hdrs++ftrs) bdy) 
                       }

        
        -- Adds a Host header if one is NOT ALREADY PRESENT
        fixHostHeader :: Request -> Request
        fixHostHeader rq =
            let uri = rqURI rq
                host = uriToAuthorityString uri
            in insertHeaderIfMissing HdrHost host rq
                                     
        -- Looks for a "Connection" header with the value "close".
        -- Returns True when this is found.
        findConnClose :: [Header] -> Bool
        findConnClose hdrs =
            case lookupHeader HdrConnection hdrs of
                Nothing -> False
                Just x  -> map toLower (trim x) == "close"

-- This function duplicates old Network.URI.authority behaviour.
uriToAuthorityString :: URI -> String
uriToAuthorityString URI{uriAuthority=Nothing} = ""
uriToAuthorityString URI{uriAuthority=Just ua} = uriUserInfo ua ++
                                                 uriRegName ua ++
                                                 uriPort ua

-- | Receive and parse a HTTP request from the given Stream. Should be used
--   for server side interactions.
receiveHTTP :: Stream s => s -> IO (Result Request)
receiveHTTP conn = do rq <- getRequestHead conn
                      case rq of
                        Left e  -> return (Left e)
                        Right r -> processRequest conn r

-- | Reads and parses request headers.
getRequestHead :: Stream s => s -> IO (Result RequestData)
getRequestHead conn =
    do { lor <- readTillEmpty1 conn
       ; return $ lor `bindE` parseRequestHead
       }

-- | Process request body (called after successful getRequestHead)
processRequest :: Stream s => s -> RequestData -> IO (Result Request)
processRequest conn (rm,uri,hdrs) =
    do -- FIXME : Also handle 100-continue.
       let tc = lookupHeader HdrTransferEncoding hdrs
           cl = lookupHeader HdrContentLength hdrs
       rslt <- case tc of
                  Nothing ->
                      case cl of
                          Just x  -> linearTransfer conn (read x :: Int)
                          Nothing -> return (Right ([], "")) -- hopefulTransfer ""
                  Just x  ->
                      case map toLower (trim x) of
                          "chunked" -> chunkedTransfer conn
                          _         -> uglyDeathTransfer conn

       return $ rslt `bindE` \(ftrs,bdy) -> Right (Request uri rm (hdrs++ftrs) bdy)


-- | Very simple function, send a HTTP response over the given stream. This 
--   could be improved on to use different transfer types.
respondHTTP :: Stream s => s -> Response -> IO ()
respondHTTP conn rsp = do writeBlock conn (show rsp)
                          -- write body immediately, don't wait for 100 CONTINUE
                          writeBlock conn (rspBody rsp)
			  return ()

-- The following functions were in the where clause of sendHTTP, they have
-- been moved to global scope so other functions can access them.		       

-- | Used when we know exactly how many bytes to expect.
linearTransfer :: Stream s => s -> Int -> IO (Result ([Header],String))
linearTransfer conn n
    = do info <- readBlock conn n
         return $ info `bindE` \str -> Right ([],str)

-- | Used when nothing about data is known,
--   Unfortunately waiting for a socket closure
--   causes bad behaviour.  Here we just
--   take data once and give up the rest.
hopefulTransfer :: Stream s => s -> String -> IO (Result ([Header],String))
hopefulTransfer conn str
    = readLine conn >>= 
      either (\v -> return $ Left v)
             (\more -> if null more 
                         then return (Right ([],str)) 
                         else hopefulTransfer conn (str++more))
-- | A necessary feature of HTTP\/1.1
--   Also the only transfer variety likely to
--   return any footers.
chunkedTransfer :: Stream s => s -> IO (Result ([Header],String))
chunkedTransfer conn
    =  chunkedTransferC conn 0 >>= \v ->
       return $ v `bindE` \(ftrs,count,info) ->
                let myftrs = Header HdrContentLength (show count) : ftrs              
                in Right (myftrs,info)

chunkedTransferC :: Stream s => s -> Int -> IO (Result ([Header],Int,String))
chunkedTransferC conn n
    =  readLine conn >>= \v -> case v of
                  Left e -> return (Left e)
                  Right line ->
                      let size = ( if null line
                                     then 0
                                     else case readHex line of
                                        (n,_):_ -> n
                                        _       -> 0
                                     )
                      in if size == 0
                           then do { rs <- readTillEmpty2 conn []
                                   ; return $
                                        rs `bindE` \strs ->
                                        parseHeaders strs `bindE` \ftrs ->
                                        Right (ftrs,n,"")
                                   }
                           else do { some <- readBlock conn size
                                   ; readLine conn
                                   ; more <- chunkedTransferC conn (n+size)
                                   ; return $ 
                                        some `bindE` \cdata ->
                                        more `bindE` \(ftrs,m,mdata) -> 
                                        Right (ftrs,m,cdata++mdata) 
                                   }                   

-- | Maybe in the future we will have a sensible thing
--   to do here, at that time we might want to change
--   the name.
uglyDeathTransfer :: Stream s => s -> IO (Result ([Header],String))
uglyDeathTransfer conn
    = return $ Left $ ErrorParse "Unknown Transfer-Encoding"

-- | Remove leading crlfs then call readTillEmpty2 (not required by RFC)
readTillEmpty1 :: Stream s => s -> IO (Result [String])
readTillEmpty1 conn =
    do { line <- readLine conn
       ; case line of
           Left e -> return $ Left e
           Right s ->
               if s == crlf
                 then readTillEmpty1 conn
                 else readTillEmpty2 conn [s]
       }

-- | Read lines until an empty line (CRLF),
--   also accepts a connection close as end of
--   input, which is not an HTTP\/1.1 compliant
--   thing to do - so probably indicates an
--   error condition.
readTillEmpty2 :: Stream s => s -> [String] -> IO (Result [String])
readTillEmpty2 conn list =
    do { line <- readLine conn
       ; case line of
           Left e -> return $ Left e
           Right s ->
               if s == crlf || null s
                 then return (Right $ reverse (s:list))
                 else readTillEmpty2 conn (s:list)
       }

        
-----------------------------------------------------------------
------------------ A little friendly funtionality ---------------
-----------------------------------------------------------------

-- | Formats name-value pairs as application\/x-www-form-urlencoded.
urlEncodeVars :: [(String,String)] -> String
urlEncodeVars xs = 
    concat $ intersperse "&" [urlEncode n ++ "=" ++ urlEncode v | (n,v) <- xs]

-- | Converts a single value to the application\/x-www-form-urlencoded encoding.
urlEncode :: String -> String
urlEncode = replace ' ' '+' . escapeURIString okChar
  where okChar c = c == ' ' || 
                   (isUnescapedInURI c && c `notElem` "&=+")

-- | Converts a single value from the 
--   application\/x-www-form-urlencoded encoding.
urlDecode :: String -> String
urlDecode = unEscapeString . replace '+' ' '

-- | Replaces all instances of a value in a list by another value.
replace :: Eq a =>
           a   -- ^ Value to look for
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replace x y = map (\z -> if z == x then y else z)
