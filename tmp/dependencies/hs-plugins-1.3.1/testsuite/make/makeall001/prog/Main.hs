
-- little more complex. use the path to the obj file we get back from
-- 'make'. load() uses this to find the .hi file

import System.Plugins
import API

main = do
        status  <- makeAll "../Tiny.hs" ["-i../api"]
        o <- case status of
                MakeSuccess _ o -> return o
                MakeFailure e -> mapM_ putStrLn e >> error "failed"
        m_v   <- load o [".."] [] "resource"
        v <- case m_v of
            LoadSuccess _ v -> return v
            _               -> error "load failed"
        putStrLn $ field v 

