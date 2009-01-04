{-# OPTIONS -fglasgow-exts #-}

module API where

data Null = Null { a, b :: Int }

null :: Null
null = Null { a = 42 , b = 1 }

