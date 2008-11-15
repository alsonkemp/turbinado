module Turbinado.Utility.General where

import Data.List
import Data.Maybe

--
-- * Utility.General
--

-- | Replaces all instances of a value in a list by another value.
replace :: Eq a =>
           a   -- ^ Value to look for
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replace x y = map (\z -> if z == x then y else z)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | Same as 'lookup' specialized to strings, but 
--   returns the empty string if lookup fails.
lookupOrNil :: String -> [(String,String)] -> String
lookupOrNil n = fromMaybe "" . lookup n

each :: [a] -> (a -> b) -> [b]
each l f = map f l
