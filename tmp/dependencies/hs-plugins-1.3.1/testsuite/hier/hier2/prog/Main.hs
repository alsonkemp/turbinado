--
-- Test if we can load a module with a hierarchical name from some weird
-- path. Tests our the module name handling in the .hi file parser.
--


module Main where

import System.Plugins

main = do
        status <- load "../A/B/C/Module.o" [".."] [] "symbol"
        case status of
                LoadFailure ers -> mapM_ putStrLn ers
                LoadSuccess _ v -> print (v :: String)
