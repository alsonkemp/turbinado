{-# OPTIONS -fglasgow-exts #-}

module API where

import Data.Dynamic

data Unsafe = Unsafe { 
                field :: String 
        }
   deriving (Typeable, Show)

unsafe :: Unsafe
unsafe = Unsafe { field = "default value" }
