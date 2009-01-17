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

-- | Used during request initialization to add the 'ViewData' 'Map
-- to the 'Environment'.
addViewDataToEnvironment :: (HasEnvironment m) => m ()
addViewDataToEnvironment  = do e <- getEnvironment
                               setEnvironment $ e {getViewData = Just (M.empty :: ViewData)}

-- | Attempts to pull a dynamically typed value out of the 'ViewData 'Map'.
-- Returns Maybe a where a is the type inferred from usage.
-- 
-- IMPORTANT: This function will return Nothing if the type inferred does not match
-- the type in the 'Map'.  So if @1 :: Int@ is stored in the 'Map' with a key "number",
-- then @getViewDataValue "number" :: 'Controller' Integer@ will return @'Controller' Nothing@.
getViewDataValue :: (HasEnvironment m, Typeable a) => String -> m (Maybe a)
getViewDataValue k = do e <- getEnvironment
                        case (M.lookup k $ fromJust $ getViewData e) of
                          Nothing -> return $ Nothing
                          Just l  -> return $ fromDynamic l

-- | This function is an "unsafe" version of 'getViewDataValue' in that this function assumes that the key
-- *does* exist in the map.  If no key exists or if the value type does not match the inferred
-- type, this function will throw an error.
getViewDataValue_u :: (HasEnvironment m, Typeable a) => String -> m a
getViewDataValue_u k = do v <- getViewDataValue k
                          maybe (error $ "getViewDataValue_u : key does not exist - \"" ++ k ++ "\"")
                                return
                                v

-- | Sets a key/value pair in the 'ViewData' 'Map'.  
--
-- IMPORTANT: the value must be
-- Typeable.  If you cannot use a Typeable (e.g. you're using a type
-- from a library), then you can extract Typeable fields from your value
-- and set those or you can convert your type to a Typeable type (e.g. using
-- 'show' to convert to a String). 
setViewDataValue :: (HasEnvironment m, Typeable a) => String -> a -> m ()
setViewDataValue k v = do e <- getEnvironment
                          let vd  = fromJust $ getViewData e
                              vd' = M.insert k (toDyn v) vd
                          setEnvironment $ e {getViewData = Just vd'}
