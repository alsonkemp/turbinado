 
import System.MkTemp

import Data.Maybe

import System.IO
import System.Directory

main = do
        createDirectory "t"

        ------------------------------------------------------------------------
        -- Try mkstemp with simple template
        --
        ts <- mapM (\_ -> mkstemp "t/t.X" ) [0..(26+26)] -- 1+26+26 files
        () <- if (not $ all isJust ts)
              then putStrLn $ "mkstemp couldn't create all expected files"
              else putStrLn $ "created "++(show $ length $ catMaybes ts)++" files"
        closeAll ts

        -- next one shouldn't be possible
        t <- mkstemp "t/t.X"
        () <- if (not $ isNothing t)
              then putStrLn $ "shouldn't have been able to create this file"
              else putStrLn $ "correctly ran out of permutations"
        closeAll [t]

        rmAll (t:ts)

        ------------------------------------------------------------------------
        -- Try again with large tmp
        --
        ts <- mapM (\_->do v <- mkstemp "t/t.XXXXXXXXXX" 
                           case v of Just (t,h) -> hClose h >> return v
                                     _ -> return v ) [1..10000]

        () <- if (not $ all isJust ts)
              then putStrLn $ "mkstemp couldn't create all expected files"
              else putStrLn $ "mkstemp:	created "++(show $ length $ catMaybes ts)++" files"
        rmAll ts

        ------------------------------------------------------------------------
        -- test mkstemps
        --
        ts <- mapM (\_->do v <- mkstemps "t/t.XXXXXXXXXX.hs" 3
                           case v of Just (t,h) -> hClose h >> return v
                                     _ -> return v ) [1..2000]
        () <- if (not $ all isJust ts)
              then putStrLn $ "mkstemps couldn't create all expected files"
              else putStrLn $ "mkstemps:	created "++(show $ length $ catMaybes ts)++" files"
        rmAll ts

        ------------------------------------------------------------------------
        -- mkdtemp
        --
        ts <- mapM (\_ -> mkdtemp "t/XXXXXXXXXX") [1..2000]
        () <- if (not $ all isJust ts)
              then putStrLn $ "mkdtemp:	couldn't create all expected directories"
              else putStrLn $ "mkdtemp:	created "++(show $ length $ catMaybes ts)++" directories"
        rmAllDirs ts
    
        ------------------------------------------------------------------------

        removeDirectory "t"

    where
        closeAll  ts = mapM_ hClose $ map snd $ catMaybes ts
        rmAll     ts = mapM_ removeFile $ map fst $ catMaybes ts
        rmAllDirs ts = mapM_ removeDirectory $ catMaybes ts
