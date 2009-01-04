import System.Plugins
import API
import Data.Either

conf    = "../Unsafe.conf"
stub    = "../Unsafe.stub"
apipath = "../api"

main = do
        status <- makeWith conf stub ["-i"++apipath]
        o <- case status of
                MakeFailure e -> mapM_ putStrLn e >> error "failed"
                MakeSuccess _ o -> return o
        m_v   <- load o [apipath] [] "resource"
        v <- case m_v of
            LoadSuccess _ v -> return v
            _               -> error "load failed"
        let s = field v
        makeCleaner o
        if null s then print False else print True
