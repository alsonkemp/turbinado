{-# GLOBALOPTIONS -package mtl #-}

module M ( resource ) where

import API
import System.IO.Unsafe
import System.Process
import System.IO

resource = tiny { field = date }

date :: String
date = unsafePerformIO $ do
            (_,outh,_,proc) <- runInteractiveProcess "echo" ["hello"] Nothing Nothing		
    	    waitForProcess proc
            s <- hGetContents outh
            return s
