module Turbinado.Environment.Types where

import Data.Dynamic
import qualified Data.Map as M
import Data.Maybe
import Data.Time
import System.IO
import System.IO.Unsafe
import System.Log.Logger
import Text.Regex
import Control.Concurrent.MVar
import Control.Monad.State
import qualified Network.HTTP as HTTP
import HSX.XMLGenerator (XMLGenT(..), unXMLGenT)
import Turbinado.View.XML
import Database.HDBC


-- | The class of types which hold an 'Environment'.
-- 'View' and 'Controller' are both instances of this class.
class (MonadIO m) => HasEnvironment m where
  getEnvironment :: m Environment
  setEnvironment :: Environment -> m ()

-- | The Environment in which each request is handled.
-- All components are held within 'Maybe's so that the
-- Environment can be partially constructed.
data Environment =  Environment { getCodeStore      :: Maybe CodeStore
                                , getDatabase       :: Maybe ConnWrapper
                                , getLoggerLock     :: Maybe LoggerLock
                                , getMimeTypes      :: Maybe MimeTypes
                                , getRequest        :: Maybe HTTP.Request
                                , getResponse       :: Maybe HTTP.Response
                                , getRoutes         :: Maybe Routes
                                , getSession        :: Maybe Session
                                , getSettings       :: Maybe Settings
                                , getViewData       :: Maybe ViewData
                                , getAppEnvironment :: Maybe (MVar Dynamic)
                                }

-- | Construct a new empty 'Environment'.
newEnvironment :: Environment
newEnvironment = Environment { getCodeStore      = Nothing
                             , getDatabase       = Nothing
                             , getLoggerLock     = Nothing
                             , getMimeTypes      = Nothing
                             , getRequest        = Nothing
                             , getResponse       = Nothing
                             , getRoutes         = Nothing
                             , getSession        = Nothing
                             , getSettings       = Nothing
                             , getViewData       = Nothing
                             , getAppEnvironment = Nothing
                             }

--
-- * Types for CodeStore
--

data CodeType = CTView | CTController | CTComponentView | CTComponentController | CTLayout deriving (Show)
type CodeDate      = UTCTime
type Function      = String
type CodeLocation  = (FilePath, Function)

data CodeStore  = CodeStore (MVar CodeMap)
type CodeMap    = M.Map CodeLocation CodeStatus
data CodeStatus = CodeLoadMissing |
                  CodeLoadFailure String |
                  CodeLoadController          (StateT Environment IO ())                 CodeDate |
                  CodeLoadView                (XMLGenT (StateT Environment IO) XML     ) CodeDate |
                  CodeLoadComponentController (StateT Environment IO ())                 CodeDate |
                  CodeLoadComponentView       (XMLGenT (StateT Environment IO) XML     ) CodeDate


--
-- * Types for Cookies
--

-- | Contains all information about a cookie set by the server.
data Cookie = Cookie {
                      -- | Name of the cookie.
                      cookieName :: String,
                      -- | Value of the cookie.
                      cookieValue :: String,
                      -- | Expiry date of the cookie. If 'Nothing', the
                      --   cookie expires when the browser sessions ends.
                      --   If the date is in the past, the client should
                      --   delete the cookie immediately.
                      cookieExpires :: Maybe UTCTime,
                      -- | The domain suffix to which this cookie will be sent.
                      cookieDomain :: Maybe String,
                      -- | The path to which this cookie will be sent.
                      cookiePath :: Maybe String
                      }
            deriving (Show, Read, Eq, Ord)

--
-- * Types for Logger
--

type LoggerLock = MVar ()


--
-- * Types for MimeTypes
--

data MimeTypes = MimeTypes (M.Map String MimeType)
data MimeType = MimeType String String

instance Show MimeType where
     showsPrec _ (MimeType part1 part2) = showString (part1 ++ '/':part2)

--
-- * Types for Request
--

-- Just a basic Request from Network.HTTP

--
-- * Types for Response
--

-- Just a basic Response from Network.HTTP

--
-- * Types for Routes
--

type Keys = [String]
data Routes = Routes [(Regex, Keys)]

--
-- * Types for Session
--
data Session = Session {
    sessionName :: Maybe String,  -- Used by Cookie session
    sessionId   :: Maybe Int,     -- Used by DB and Filesystem sessions
    expires :: Maybe UTCTime,
    dataRep :: M.Map String String
    } deriving (Eq, Read, Show)

emptySession = Session Nothing Nothing Nothing M.empty

class HasSession m where
  newSession      :: [(String, String)] -> m ()
  retrieveSession :: [(String, String)] -> m ()
  persistSession  :: [(String, String)] -> m ()
  hasValidSession  :: m Bool
  abandonSession  :: m ()
  getSessionValue :: String -> m (Maybe String)
  setSessionValue :: String -> String -> m ()
  deleteSessionKey :: String -> m ()
  getSessionId   :: m (Maybe Int)
  setSessionId   :: Maybe Int -> m ()
  getSessionExpires :: m (Maybe UTCTime)
  setSessionExpires :: Maybe UTCTime -> m ()

--
-- * Types for Settings
--

type Settings = M.Map String Dynamic

--
-- * Types for ViewData
--

type ViewData = M.Map String Dynamic

