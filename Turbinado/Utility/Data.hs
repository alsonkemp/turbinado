module Turbinado.Utility.Data where

import Data.Maybe

fromJust' :: String -> Maybe a -> a
fromJust' s = fromMaybe (error $ "fromJust' called with Nothing : " ++ s)
