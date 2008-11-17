module Turbinado.Environment.Settings (
        addSettingsToEnvironment,
        getSetting,
        setSetting,
        getSettings,
        setSettings,
        getController,
        clearLayout,
        getLayout,
        getView
        )where

import Data.Dynamic
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Char
import System.FilePath
import Turbinado.Environment

type Settings = M.Map String Dynamic


settingsKey = "settings"

addSettingsToEnvironment :: EnvironmentFilter
addSettingsToEnvironment  = setSettings (M.fromList defaultSettings :: Settings)

getSettings :: Environment -> Settings
getSettings  = getKey settingsKey

setSettings :: Settings -> EnvironmentFilter
setSettings = setKey settingsKey

------------------------------------------------------------------
-- Set/Get an individual settting
------------------------------------------------------------------
getSetting :: (Typeable a) => String -> Environment -> Maybe a
getSetting s e = maybe Nothing (fromDynamic) ( M.lookup s (getKey settingsKey e) )

getSetting_u :: (Typeable a) => String -> Environment -> a
getSetting_u s e = fromJust (getSetting s e)

setSetting :: (Typeable a) => String -> a -> EnvironmentFilter
setSetting k v e = do let settings = getSettings e
                      setSettings (M.insert k (toDyn v) settings) e

defaultSettings = [ ("layout", toDyn "Default") ]

------------------------------------------------------------------
-- Shorthands
------------------------------------------------------------------
getController :: Environment -> (FilePath, String)
getController e = (             fromJust $ getSetting "controller" e,
                   actionName $ fromJust $ getSetting "action" e)
                    where actionName s = (toLower $ head s) : (tail s)

clearLayout :: EnvironmentFilter
clearLayout = setSetting "layout" ""

getLayout :: Environment -> (FilePath, String)
getLayout e = (fromJust $ getSetting "layout" e, "page")

getView :: Environment -> (FilePath, String)
getView e = let c = fromJust $ getSetting "controller" e
                a = fromJust $ getSetting "action" e
            in (joinPath $ map normalise [c,a], "page")

