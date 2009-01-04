{-# OPTIONS -cpp #-}

#include "../../../../config.h"

import System.Plugins
import API

main = do 
        m_v <- dynload "../Plugin.o" ["../api"]
                                     []
                                     "resource_dyn"
        case m_v of
                LoadFailure _   -> error "didn't compile"
                LoadSuccess _ v -> putStrLn $ (function v)

