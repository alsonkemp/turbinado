module Turbinado.Utility.Data where

import Data.Maybe

fromJust' :: String -> Maybe a -> a
fromJust' s m = maybe
                   (error $ "fromJust' called with Nothing : " ++ s)
                   id
                   m
