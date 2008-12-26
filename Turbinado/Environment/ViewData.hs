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

import Turbinado.Environment.Logger
import Turbinado.Environment.Types

addViewDataToEnvironment :: (HasEnvironment m) => m ()
addViewDataToEnvironment  = do e <- getEnvironment
                               setEnvironment $ e {getViewData = Just (M.empty :: ViewData)}

getViewDataValue :: (HasEnvironment m, Typeable a) => String -> m (Maybe a)
getViewDataValue k = do e <- getEnvironment
                        case (M.lookup k $ fromJust $ getViewData e) of
                          Nothing -> return $ Nothing
                          Just l  -> return $ fromDynamic l

getViewDataValue_u :: (HasEnvironment m, Typeable a) => String -> m a
getViewDataValue_u k = do v <- getViewDataValue k
                          return $ fromJust v

setViewDataValue :: (HasEnvironment m, Typeable a) => String -> a -> m ()
setViewDataValue k v = do e <- getEnvironment
                          let vd  = fromJust $ getViewData e
                              vd' = M.insert k (toDyn v) vd
                          setEnvironment $ e {getViewData = Just vd'}
