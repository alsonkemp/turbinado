
import System.Plugins
import API

-- an example where we just want to load an object and run it

main = do 
    m_v   <- load_ "../Null.o" ["../api",".."] "resource"
    t     <- load_ "../Dep.o" ["../api"] "resource"
    case m_v of
        LoadFailure err -> error (unlines err)
        LoadSuccess m v -> do putStrLn ( show (a v) ) ; unloadAll m -- unloads Null.o but not Dep.o since we're still using it.
    case t of
        LoadFailure err -> error (unlines err)
        LoadSuccess m v -> do putStrLn ( show (a v) ) ; unloadAll m
