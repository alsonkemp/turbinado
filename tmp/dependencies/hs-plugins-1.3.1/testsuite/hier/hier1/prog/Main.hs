--
-- Test multiple plugins
--


module Main where

import System.Plugins
import API
import Modules.Flags as Flags


rec = Flags.FlagRec { Flags.f1 = 4, Flags.f2 = 10 }


main = do
        status <- load "../Plugin.o" ["../api",".."] [] "resource"
        case status of
                LoadFailure _ -> error "load failed"
                LoadSuccess _ v -> do let func = dbFunc v
                                      print (func rec)
