
import System.Plugins
import API

--
-- what happens if we try to use code that has been unloaded?
--

main = do 
    m_v   <- load "../Null.o" ["../api"] [] "resource"
    (m,v) <- case m_v of
        LoadSuccess m v -> return (m,v)
        _               -> error "load failed"
    putStrLn ( show (a v) )
    unload m
