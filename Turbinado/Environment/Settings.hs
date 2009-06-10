module Turbinado.Environment.Settings (
        addSettingsToEnvironment,
        getSetting,
        getSetting_u,
        setSetting,
        getController,
        clearLayout,
        setLayout,
        getView
        )where

import Data.Dynamic
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Char
import System.FilePath
import Turbinado.Environment.Logger
import Turbinado.Environment.Types
import Turbinado.Controller.Monad
import Turbinado.Utility.Naming
import Turbinado.Utility.Data
import qualified Config.Master as Config

-- | Used during request initialization to add the 'Settings' 'Map'
-- to the 'Environment'.
addSettingsToEnvironment :: (HasEnvironment m) => m ()
addSettingsToEnvironment  = do e <- getEnvironment
                               setEnvironment $ e {getSettings = Just $ M.fromList defaultSettings }

------------------------------------------------------------------
-- Set/Get an individual settting
------------------------------------------------------------------

-- | Attempts to pull a dynamically typed value out of the 'Settings' 'Map'.
-- Returns @Maybe a@ where @a@ is the type inferred from usage.
--
-- IMPORTANT: This function will return Nothing if the type inferred does not match
-- the type in the 'Map'.  So if @1 :: Int@ is stored with a key "number",
-- then @getSetting "number" :: 'Controller' Integer@ will return @'Controller' Nothing@.
getSetting :: (HasEnvironment m, Typeable a) => String -> m (Maybe a)
getSetting s = do e <- getEnvironment
                  return $ maybe Nothing (fromDynamic) ( M.lookup s (fromJust' "Settings : getSetting" $ getSettings e) )

-- | This function is an "unsafe" version of 'getSetting' in that this function assumes that the key
-- *does* exist in the map.  If no key exists or if the value type does not match the inferred
-- type, this function will throw an error.
getSetting_u :: (HasEnvironment m) => String -> m String
getSetting_u s = do v <- getSetting s
                    maybe (error $ "getSetting_u : key does not exist - \"" ++ s ++ "\"")
                          return
                          v
                          
-- | Sets a key/value pair in the 'Settings' map.  
--
-- IMPORTANT: the value must be
-- Typeable.  If you cannot use a Typeable (e.g. you're using a type
-- from a library), then you can extract Typeable fields from your value
-- and set those or you can convert your type to a Typeable type (e.g. using
-- 'show' to convert to a String). 
setSetting :: (HasEnvironment m, Typeable a) => String -> a -> m ()
setSetting k v = do e <- getEnvironment
                    setEnvironment $ e { getSettings = Just (M.insert k (toDyn v) (fromJust' "Settings : setSetting" $ getSettings e))}

-- | Unsets a setting.  If the key does not exist, no error is thrown.
unsetSetting :: (HasEnvironment m) => String -> m ()
unsetSetting k = do e <- getEnvironment
                    setEnvironment $ e { getSettings = Just (M.delete k (fromJust' "Settings : unsetSetting" $ getSettings e))}

-- ! The 'Settings' to use at the start of each request.
defaultSettings :: [(String, Dynamic)]
defaultSettings = [ ("layout", toDyn "Default") ]

------------------------------------------------------------------
-- Shorthands
------------------------------------------------------------------
-- | Returns the Controller FilePath and action/function name.
getController :: (HasEnvironment m) => m (FilePath, String)
getController = do c <- getSetting_u "controller"
                   a <- getSetting_u "action"
                   let converter = if Config.useLowerCasePaths
                                        then fromUnderscore
                                        else id
                   return $ (converter c,
                             actionName $ converter a)
                               where actionName s = (toLower $ head s) : (tail s)

-- | Tells the 'Controller' to use a particular 'Layout' for the 'View'.
setLayout :: (HasEnvironment m) => String -> m ()
setLayout l = setSetting "layout" l

-- | Tells the 'Controller' not to use a 'Layout' for the 'View'.
clearLayout :: (HasEnvironment m) => m ()
clearLayout = unsetSetting "layout"

-- | Helper function used by the request handler.
getView :: (HasEnvironment m) => m (FilePath, String)
getView = do c <- getSetting_u "controller"
             a <- getSetting_u "action"
             let converter = if Config.useLowerCasePaths
                                  then fromUnderscore
                                  else id
             return (joinPath $ map normalise [converter c, converter a], "markup")

