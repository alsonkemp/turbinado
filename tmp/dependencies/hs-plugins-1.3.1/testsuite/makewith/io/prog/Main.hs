
import System.Plugins
import API

conf    = "../TestIO.conf"
stub    = "../TestIO.stub"
apipath = "../api"

main = do
        status <- makeWith conf stub ["-i"++apipath]
        o <- case status of
                MakeFailure e   -> mapM_ putStrLn e >> error "failed"
                MakeSuccess _ o -> return o
        m_v   <- load o [apipath] [] "resource"
        v <- case m_v of
            LoadSuccess _ v -> return v
            _               -> error "load failed"
        s <- field v

        makeCleaner o
        if null s then print False else print True
