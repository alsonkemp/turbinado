
import System.Plugins
import API

conf    = "../Plugin.hs"
apipath = "../api"

main = do
        status <- makeWith conf conf ["-i"++apipath]
        o <- case status of
                MakeFailure e   -> mapM_ putStrLn e >> error "compile failed"
                MakeSuccess _ o -> return o
        m_v   <- load o [apipath] [] "resource"
        v <- case m_v of
            LoadSuccess _ v -> return v
            LoadFailure ers -> mapM_ putStrLn ers >> error "load failed"
        putStr $ field v 
        makeCleaner o

