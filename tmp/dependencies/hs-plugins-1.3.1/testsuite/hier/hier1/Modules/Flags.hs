--
-- A simple module
--

module Modules.Flags where


data FlagRec = FlagRec {
        f1 :: Int,
        f2 :: Int
}


foo :: FlagRec -> Int
foo x = f1 x
