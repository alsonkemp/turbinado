{-# OPTIONS -fglasgow-exts #-}

module API where

data Null = Null { a, b :: Int }
   deriving Show

plugin :: Null
plugin = Null { a = 42 , b = 1 }

