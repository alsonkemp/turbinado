
import System.Plugins

import System.Directory

a    = "Foo.hs" -- uesr code
b    = "Bar.hs" -- trusted code. Result is "Bar.o"

main = do
        status <- merge a b 
        f <- case status of
                MergeFailure e     -> error "merge failure"
                MergeSuccess _ _ f -> return f

        status <- merge a b 
        f' <- case status of
                MergeFailure e          -> error "merge failure"
                MergeSuccess ReComp _ f -> error "unnec. merge"
                MergeSuccess NotReq _ f -> return f

        print ( f == f' )

        status <- make f' []
        o <- case status of
                  MakeFailure   e -> error "make failed"
                  MakeSuccess _ o -> return o

        m_v   <- load o [] [] "resource"
        v <- case m_v of
            LoadSuccess _ v -> return v
            _               -> error "load failed"
        putStrLn $ show $ (v :: Int)

        removeFile o
        return ()

        makeCleaner f

