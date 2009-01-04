{-# OPTIONS -cpp #-}

#include "../../../../config.h"

import System.Plugins
import API

main = do 
    let includes = TOP ++ "/testsuite/load/null/api"
    (_,v) <- load "../Null.o" ["."] ["../api/package.conf"] "resource"
    putStrLn ( show (a v) )

