
import System.Plugins
import API

main = do
        status <- make "../Test.hs" ["-i../api"]
        obj    <- case status of
                    MakeSuccess _ o -> return o
                    MakeFailure e   -> mapM_ putStrLn e >> error "failed"

        m_v <- load_ obj ["../api"] "resource"
        v <- case m_v of
                LoadFailure _   -> error "load failed"
                LoadSuccess _ v -> return v
        let s = field v
        print s
