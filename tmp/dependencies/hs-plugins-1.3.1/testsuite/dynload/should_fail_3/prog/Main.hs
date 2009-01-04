
import System.Plugins
import API

conf    = "../Plugin.in"
stub    = "../Plugin.stub"

main = do 
        status <- makeWith conf stub ["-i../api", "-i../../../../src/altdata"]
        o <- case status of
                MakeFailure e -> mapM_ putStrLn e >> error "failed"
                MakeSuccess _ o -> return o
        m_v <- dynload o ["../api"] [] "resource_dyn"
        case m_v of
                LoadFailure _   -> error "didn't compile"
                LoadSuccess _ v -> do putStrLn $ (function v)
                                      makeCleaner o

