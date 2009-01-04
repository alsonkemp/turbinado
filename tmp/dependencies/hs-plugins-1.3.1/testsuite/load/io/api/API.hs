{-# OPTIONS -fglasgow-exts #-}

module API where

import Data.Dynamic

data TestIO = TestIO { 
                field :: IO String 
        }
   deriving (Typeable, Show)

instance Show (IO String) where
    show _ = "<<io action>>"

testio :: TestIO
testio = TestIO { field = return "default value" }
