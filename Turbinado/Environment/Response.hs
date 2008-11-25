module Turbinado.Environment.Response (
        HTTP.Response,
        addResponseToEnvironment,
        getResponse,
        setResponse,
        isResponseComplete
        )where

import qualified Network.HTTP as HTTP
import Network.URI
import Turbinado.Utility.General
import qualified Data.Map as M
import Control.Monad
import Data.Maybe
import {-# SOURCE #-} Turbinado.Environment
import System.Time
import System.Locale


-- | Build a Response  object given a parsed HTTP request.
addResponseToEnvironment :: EnvironmentFilter
addResponseToEnvironment e = do 
    t <- getClockTime
    setResponse (HTTP.Response (0,0,0) "" (startingHeaders t) "") e

startingHeaders t = [ HTTP.Header HTTP.HdrServer "Turbinado www.turbinado.org"
                    , HTTP.Header HTTP.HdrContentType "text/html; charset=UTF-8"
                    , HTTP.Header HTTP.HdrDate $ formatCalendarTime defaultTimeLocale rfc822DateFormat $ toUTCTime t
                    ]

responseKey = "response"

getResponse :: Environment -> HTTP.Response
getResponse = getKey responseKey

setResponse :: HTTP.Response -> EnvironmentFilter
setResponse = setKey responseKey

isResponseComplete :: Environment -> Bool
isResponseComplete e = let r = getResponse e
                       in (HTTP.rspCode r /= (0,0,0))

{-
lookupHeader :: (Monad m) => m (Maybe String)
lookupHeader = liftM . lookupHeader

lookupHeaderWithDefault :: (Monad m) => HTTP.Header -> String -> m String
lookupHeaderWithDefault h s = do s' <- (liftM . lookupHeader) h
                                 case s' of
                                        Nothing  -> s
                                        Just s'' -> s''
-}

unEscape s = unEscapeString $ map (\ch -> if ch == '+' then ' ' else ch) s

--
-- * Environment variables
--

{-
-- | Get the value of a Controller environment variable. Example:
--
-- > remoteAddr <- getVar "REMOTE_ADDR"
getVar :: (Monad m) => 
          String             -- ^ The name of the variable.
       -> m (Maybe String)
getVar name = liftM (M.lookup name $ inputs)

getVarWithDefault :: (Monad m) => 
                     String -- ^ The name of the variable.
                  -> String -- ^ Default value 
                  -> m String
getVarWithDefault name def = liftM (fromMaybe def) $ getVar name

--
-- * Inputs
--

-- | Get the value of an input variable, for example from a form.
--   If the variable has multiple values, the first one is returned.
--   Example:
--
-- > query <- getInput "query"
getInput :: (Monad m) => 
            String           -- ^ The name of the variable.
         -> m (Maybe String) -- ^ The value of the variable,
                             --   or Nothing, if it was not set.
getInput v = lookup v `liftM` (request . getRequest)

-- | Like 'getInput', but returns a 'String'.
getInputFPS :: (Monad m) => 
               String           -- ^ The name of the variable.
            -> m (Maybe String) -- ^ The value of the variable,
                             --   or Nothing, if it was not set.
getInputFPS = liftM (fmap inputValue) . getInput_


-- | Get the value of an input variable or a default value if the
--   the input variable is not found.
--   Example:
--
-- > query <- getInput "somevariable" "defaultvalue" 
getInputWithDefault :: (Monad m) => 
                       String           -- ^ The name of the variable.
                    -> String           -- ^ The default value.
                    -> m String         -- ^ The value of the variable or default
getInputWithDefault v s = do v' <- getInput v
                             case v'
                               of Nothing -> s
                                  Just s' -> s'

-- | Same as 'getInput', but tries to read the value to the desired type.
readInput :: (Read a, Monad m) =>
             String        -- ^ The name of the variable.
          -> m (Maybe a) -- ^ 'Nothing' if the variable does not exist
                           --   or if the value could not be interpreted
                           --   at the desired type.
readInput = liftM (>>= maybeRead) . getInput

-- | Same as 'readInput', but with a default value.
readInputWithDefault :: (Read a, Monad m) =>
                        String         -- ^ The name of the variable.
                        -> a           -- ^ The default value
                        -> m (Maybe a) -- ^ 'Nothing' if the variable does not exist
                                       --   or if the value could not be interpreted
                                       --   at the desired type.
readInputWithDefault v d = do v' <- liftM (>>= maybeRead) . getInput
                              case v' of Nothing -> d
                                         Just v'' -> v''

-}

{-
-- | Get the names and values of all inputs.
--   Note: the same name may occur more than once in the output,
--   if there are several values for the name.
parseInputs :: (Monad m) => HTTP.Request -> m (M.Map String String)
parseInputs r = do is <- r
                   return M.fromList $ [ (n, inputValue i) | (n,i) <- is ]

-- Internal stuff

getInput_ ::  (Monad m) => String -> m (Maybe Input)
getInput_ n = lookup n `liftM` getRequest

-- | Get the uninterpreted request body as a String
getBody :: (Monad m) => m String
getBody = liftM (HTTP.rqBody . httpRequest) getRequest

-}
