
import System.Plugins
import API

conf    = "../Mailrc.conf"
stub    = "../Mailrc.stub"
apipath = "../api"

main = do
        status <- makeWith conf stub ["-i"++apipath]
        o <- case status of
                MakeFailure e   -> mapM_ putStrLn e >> error "failed"
                MakeSuccess _ o -> return o
        status <- load o [apipath] [] "resource"
        v <- case status of
                LoadFailure err   -> mapM_ putStrLn err >> error "no"
                LoadSuccess _ v -> return v
        
        user_editor <- editor v
        putStrLn user_editor
        makeCleaner o

