
import System.Plugins
import API

import A

main = do
        -- compile C (A and B are already compiled)
        status <- makeAll "../C.hs" ["-i../api"]
        obj    <- case status of
                    MakeSuccess _ o -> return o
                    MakeFailure e   -> mapM_ putStrLn e >> error "failed"

        -- should load C
        m_v <- load_ obj ["../api","."] "resource"
        v <- case m_v of
                LoadFailure _   -> error "load failed"
                LoadSuccess _ v -> return v
        let s = field v
        print s
