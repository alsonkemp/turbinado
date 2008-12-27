module Turbinado.Environment.Types where

import Data.Dynamic
import qualified Data.Map as M
import Data.Maybe
import System.IO
import System.IO.Unsafe
import System.Log.Logger
import Text.Regex
import Control.Concurrent.MVar
import Control.Monad.State
import qualified Network.HTTP as HTTP
import HSX.XMLGenerator (XMLGenT(..), unXMLGenT)
import Turbinado.View.XML
import Config.Master
import System.Time
import System.Plugins

class (MonadIO m) => HasEnvironment m where
  getEnvironment :: m Environment
  setEnvironment :: Environment -> m ()

-- Stuffing all Environment "types" into this file to avoid
-- recursive imports...

data Environment = Environment { getCodeStore      :: Maybe CodeStore
                               , getDatabase       :: Maybe Database
                               , getLoggerLock     :: Maybe LoggerLock
                               , getMimeTypes      :: Maybe MimeTypes
                               , getRequest        :: Maybe HTTP.Request
                               , getResponse       :: Maybe HTTP.Response
                               , getRoutes         :: Maybe Routes
                               , getSettings       :: Maybe Settings
                               , getViewData       :: Maybe ViewData
                               , getAppEnvironment :: Maybe AppEnvironment
                               }

-- type EnvironmentFilter = Environment -> IO Environment

newEnvironment :: Environment
newEnvironment = Environment { getCodeStore      = Nothing
                             , getDatabase       = Nothing
                             , getLoggerLock     = Nothing
                             , getMimeTypes      = Nothing
                             , getRequest        = Nothing
                             , getResponse       = Nothing
                             , getRoutes         = Nothing
                             , getSettings       = Nothing
                             , getViewData       = Nothing
                             , getAppEnvironment = Nothing
                             }

--
-- * Types for CodeStore
--

data CodeType = CTView | CTController | CTComponentView | CTComponentController | CTLayout deriving (Show)
type CodeDate      = ClockTime
type Function      = String
type CodeLocation  = (FilePath, Function)

data CodeStore  = CodeStore (MVar CodeMap)
type CodeMap    = M.Map CodeLocation CodeStatus
data CodeStatus = CodeLoadMissing |
                  CodeLoadFailure String |
                  CodeLoadController          (StateT Environment IO ())                 Module CodeDate |
                  CodeLoadView                (XMLGenT (StateT Environment IO) XML     ) Module CodeDate |
                  CodeLoadComponentController (StateT Environment IO ())                 Module CodeDate |
                  CodeLoadComponentView       (XMLGenT (StateT Environment IO) XML     ) Module CodeDate

--
-- * Types for Database
--

type Database = Connection


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
-- * Types for Settings
--

type Settings = M.Map String Dynamic

--
-- * Types for ViewData
--

type ViewData = M.Map String Dynamic

