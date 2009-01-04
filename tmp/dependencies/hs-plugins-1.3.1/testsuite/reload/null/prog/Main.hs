
import System.Plugins
import API

-- an example where we just want to load an object and run it

main = do 
    m_v <- load "../Null.o" ["../api"] [] "resource"
    (m,v) <- case m_v of
        LoadSuccess m v -> return (m,v)
        _               -> error "load failed"
    putStrLn ( show (a v) )

    m_v <- reload m "resource"   -- get a new version
    v' <- case m_v of
        LoadSuccess _ v -> return v
        _               -> error "load failed"
    putStrLn ( show (a v') )

