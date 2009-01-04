{-# OPTIONS -fglasgow-exts #-}

module API where

import Data.Dynamic

data Null = Null { a, b :: Int }
   deriving (Typeable, Show)

null :: Null
null = Null { a = 42 , b = 1 }

