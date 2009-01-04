{-# OPTIONS -cpp #-}

#include "../../../../config.h"

import System.Plugins
import API

main :: IO ()
main = do
        m_v <- load "../TestIO.o" ["../api"] [] "resource"
        v <- case m_v of
                LoadFailure _   -> error "load failed"
                LoadSuccess _ v -> return v
        s <- field v
        if null s then print False else print True
