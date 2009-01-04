
import System.Plugins
import API

conf    = "../Plugin.in"
stub    = "../Plugin.stub"

main = do 
        status <- makeWith conf stub ["-i../api"]
        case status of
                MakeFailure e    -> putStrLn "make failed"
                MakeSuccess _  o ->  do
                        m_v   <- load o ["../api"] [] "resource"
                        v     <- case m_v of
                                    LoadSuccess _ v -> return v
                                    _               -> error "load failed"
                        putStrLn $ (function v)
                        makeCleaner o

