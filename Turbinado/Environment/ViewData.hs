module Turbinado.Environment.ViewData (
        addViewDataToEnvironment,
        getViewDataValue,
        getViewDataValue_u,
        setViewDataValue
        )where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Data.Typeable
import Data.Dynamic

import Turbinado.Environment.Types
import Turbinado.Controller.Monad
import Turbinado.View.Monad

addViewDataToEnvironment :: Controller ()
addViewDataToEnvironment  = do e <- get
                               put $ e {getViewData = Just (M.empty :: ViewData)}

getViewDataValue :: (Typeable a) => String -> View (Maybe a)
getViewDataValue k = do e <- lift get
                        case (M.lookup k $ fromJust $ getViewData e) of
                          Nothing -> return $ Nothing
                          Just l  -> return $ fromDynamic l

getViewDataValue_u :: (Typeable a) => String -> View a
getViewDataValue_u k = do v <- getViewDataValue k
                          return $ fromJust v

setViewDataValue :: (Typeable a) => String -> a -> Controller ()
setViewDataValue k v = do e <- get
                          let vd  = fromJust $ getViewData e
                              vd' = M.insert k (toDyn v) vd
                          put $ e {getViewData = Just vd'}
