module Main where

import System.Plugins

main = do

    makeAll "A.hs" []

    y <- load "A.o" ["."] [] "u"
    case y of
        LoadSuccess _ _ -> putStrLn $ "YES"
        LoadFailure e   -> mapM_ putStrLn e
