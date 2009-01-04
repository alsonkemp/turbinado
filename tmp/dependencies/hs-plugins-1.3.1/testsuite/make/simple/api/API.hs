{-# OPTIONS -fglasgow-exts #-}
-- ^ needed to derive Typeable

module API where

import Data.Dynamic

data Tiny = Tiny { field :: String }
   deriving (Typeable, Show)

tiny :: Tiny
tiny = Tiny { field = "default value" }

