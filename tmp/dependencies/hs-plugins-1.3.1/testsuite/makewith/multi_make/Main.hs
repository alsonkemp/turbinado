
import System.Plugins

import System.Directory

a    = "Foo.hs" -- user code
b    = "Bar.hs" -- more user code
z    = "Stub.hs" -- and a stub

main = do
        status <- makeWith a z []
        s <- case status of
                MakeFailure   e -> mapM_ putStrLn e >> error "failed"
                MakeSuccess n s -> print n >> return s

        status <- makeWith b z []
        s' <- case status of
                MakeFailure   e -> mapM_ putStrLn e >> error "failed"
                MakeSuccess n s -> print n >> return s

        -- shouldn't need to remerge (a,z)
        status <- makeWith a z []
        t <- case status of
                MakeFailure   e -> mapM_ putStrLn e >> error "failed"
                MakeSuccess n s -> print n >> return s

        -- shouldn't need to remerge (b,z)
        status <- makeWith b z []
        t' <- case status of
                MakeFailure   e -> mapM_ putStrLn e >> error "failed"
                MakeSuccess n s -> print n >> return s

        print $ s /= s' -- test we got unique modules
        print $ t /= t' -- test we got unique modules

        mapM_ makeCleaner [s,s']

