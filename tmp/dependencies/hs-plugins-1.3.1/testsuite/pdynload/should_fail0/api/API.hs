{-# OPTIONS -fglasgow-exts #-}
-- ^ needed to derive Typeable

module API where

import Data.Dynamic

data Interface = Interface { field :: String }
   deriving (Show)

rsrc :: Interface
rsrc = Interface { field = "default value" }

