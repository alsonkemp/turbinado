module Turbinado.Environment.Settings (
        addSettingsToEnvironment,
        getSetting,
        getSetting_u,
        setSetting,
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
import Turbinado.Environment.Types
import Turbinado.Controller.Monad

addSettingsToEnvironment :: (HasEnvironment m) => m ()
addSettingsToEnvironment  = do e <- getEnvironment
                               setEnvironment $ e {getSettings = Just $ M.fromList defaultSettings }

------------------------------------------------------------------
-- Set/Get an individual settting
------------------------------------------------------------------
getSetting :: (HasEnvironment m, Typeable a) => String -> m (Maybe a)
getSetting s = do e <- getEnvironment
                  return $ maybe Nothing (fromDynamic) ( M.lookup s (fromJust $ getSettings e) )

getSetting_u s =  getSetting s >>= \v -> return (fromJust v)

setSetting :: (HasEnvironment m, Typeable a) => String -> a -> m ()
setSetting k v = do e <- getEnvironment
                    setEnvironment $ e { getSettings = Just (M.insert k (toDyn v) (fromJust $ getSettings e))}

defaultSettings = [ ("layout", toDyn "Default") ]

------------------------------------------------------------------
-- Shorthands
------------------------------------------------------------------
getController :: (HasEnvironment m) => m (FilePath, String)
getController = do e <- getEnvironment
                   c <- getSetting "controller"
                   a <- getSetting "action"
                   return $ (fromJust c,
                             actionName $ fromJust a)
                               where actionName s = (toLower $ head s) : (tail s)

clearLayout :: (HasEnvironment m) => m ()
clearLayout = setSetting "layout" ""

getLayout :: (HasEnvironment m) => m (FilePath, String)
getLayout = (\l -> return (fromJust l, "markup")) =<< getSetting "layout"

getView :: (HasEnvironment m) => m (FilePath, String)
getView = do c <- getSetting_u "controller"
             a <- getSetting_u "action"
             return (joinPath $ map normalise [c,a], "markup")

