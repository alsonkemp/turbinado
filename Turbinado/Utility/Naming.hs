module Turbinado.Utility.Naming where

import Data.Char
import Data.List

--
-- * Turbinado Utility functions for Naming 
--

-- | Lowercases the first letter to make a valid function.
underscoreToFunction [] = error "toFunction passed an empty string"
underscoreToFunction (firstL:ls) = (Data.Char.toLower firstL) : fromUnderscore ls


-- | Uppercases the first letter to make a valid type.
underscoreToType [] = error "toType passed an empty string"
underscoreToType l = fromUnderscore l

-- | Convert AbbaDing to abba_ding
toUnderscore [] = error "toUnderscore passed an empty string"
toUnderscore (l:ls) = toLower l : worker ls
  where worker [] = []
        worker "_"        = "_" -- end with "_", then end with "_"
        worker (c:cs)     | isUpper c  = '_' : toLower c : worker cs
                          | otherwise  = c : worker cs

-- | Convert abba_ding to AbbaDing
fromUnderscore [] = error "fromUnderscore passed an empty string"
fromUnderscore (l:ls) = toUpper l : worker ls
  where worker [] = []
        worker  "_"       = "_" -- end with "_", then end with "_"
        worker ('_':c:cs) | isLetter c = toUpper c : worker cs
                          | otherwise  = '_'   : c : worker cs
        worker (c:cs)     = c : worker cs

