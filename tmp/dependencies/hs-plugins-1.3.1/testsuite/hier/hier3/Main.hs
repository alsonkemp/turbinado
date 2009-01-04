module Main where

import System.Plugins

main = do

    makeAll "One.hs" []

    load2 "Two.o"

    load2 "./Two.o"         -- shouldn't load
    load2 "../hier3/Two.o"  -- shouldn't load
    load2 "././././Two.o"   -- shouldn't load

    -- and this one pulls in "../hier3/Two.o" as a dep
    y <- load "One.o" ["../hier3"] [] "resource"
    case y of
        LoadSuccess _ s -> putStrLn $ "One plugin: " ++ s
        LoadFailure _   -> putStrLn "Failure: y"

load2 f = do
    x <- load f [".", "../hier3", ""] [] "resource"   -- depend on One.o
    case x of
        LoadSuccess _ s -> putStrLn $ "Two plugin: " ++ s
        LoadFailure _   -> putStrLn "Failure: x"
