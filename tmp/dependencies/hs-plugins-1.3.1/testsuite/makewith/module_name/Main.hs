
import System.Plugins

import System.Directory

a    = "Foo.hs" -- uesr code
b    = "Bar.hs" -- trusted code. Result is "Bar.o"

main = do
        status <- makeWith a b []
        s <- case status of
                MakeFailure   e -> mapM_ putStrLn e >> error "failed"
                MakeSuccess n s -> print n >> return s

        status <- makeWith a b []
        s' <- case status of
                MakeFailure   e -> mapM_ putStrLn e >> error "failed"
                MakeSuccess n s -> print n >> return s

        status <- makeWith a b []
        s'' <- case status of
                MakeFailure   e -> mapM_ putStrLn e >> error "failed"
                MakeSuccess n s -> print n >> return s

        print $ (s == s') && (s' == s'')

        m_v   <- load s [] [] "resource"
        v <- case m_v of
            LoadSuccess _ v -> return v
            _               -> error "load failed"
        putStrLn $ show $ (v :: Int)

        makeCleaner s''
