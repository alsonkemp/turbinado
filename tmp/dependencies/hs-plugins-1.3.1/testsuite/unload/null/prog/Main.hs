
import System.Plugins
import API

-- an example where we just want to load an object and run it

main = do 
    m_v   <- load "../Null.o" ["../api"] [] "resource"
    case m_v of
        LoadFailure _   -> error "load failed"
        LoadSuccess m v -> do putStrLn ( show (a v) ) ; unload m
