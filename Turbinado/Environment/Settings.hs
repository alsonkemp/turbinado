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

addSettingsToEnvironment :: Controller ()
addSettingsToEnvironment  = do e <- get
                               put $ e {getSettings = Just $ M.fromList defaultSettings }

------------------------------------------------------------------
-- Set/Get an individual settting
------------------------------------------------------------------
getSetting :: Typeable a => String -> Controller (Maybe a)
getSetting s = do e <- get
                  return $ maybe Nothing (fromDynamic) ( M.lookup s (fromJust $ getSettings e) )

getSetting_u s =  getSetting s >>= \v -> return (fromJust v)

setSetting :: (Typeable a) => String -> a -> Controller ()
setSetting k v = do e <- get
                    put $ e { getSettings = Just (M.insert k (toDyn v) (fromJust $ getSettings e))}

defaultSettings = [ ("layout", toDyn "Default") ]

------------------------------------------------------------------
-- Shorthands
------------------------------------------------------------------
getController :: Controller (FilePath, String)
getController = do e <- get
                   c <- getSetting "controller"
                   a <- getSetting "action"
                   return $ (fromJust c,
                             actionName $ fromJust a)
                               where actionName s = (toLower $ head s) : (tail s)

clearLayout :: Controller ()
clearLayout = setSetting "layout" ""

getLayout :: Controller (FilePath, String)
getLayout = (\l -> return (fromJust l, "page")) =<< getSetting "layout"

getView :: Controller (FilePath, String)
getView = do c <- getSetting_u "controller"
             a <- getSetting_u "action"
             return (joinPath $ map normalise [c,a], "page")

