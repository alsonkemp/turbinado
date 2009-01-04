import System.Plugins
import API

main = do
        let plist = ["../Plugin1.o", "../Plugin2.o", "../Plugin3.o"]
        plugins <- mapM (\p -> load p ["../api"] [] "resource") plist
        let functions = map (valueOf . fromLoadSuc) plugins

        -- apply the function from each plugin in turn
        mapM_ (\f -> putStrLn $ f "haskell is for hackers") functions

fromLoadSuc (LoadFailure _)   = error "load failed"
fromLoadSuc (LoadSuccess _ v) = v
