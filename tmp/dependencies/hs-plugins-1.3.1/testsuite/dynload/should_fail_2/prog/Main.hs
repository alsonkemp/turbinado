
import System.Plugins
import API

conf    = "../Plugin.in"
stub    = "../Plugin.stub"

main = do 
        status <- makeWith conf stub ["-i../api", "-i../../../../src/altdata/"]
        case status of
         MakeFailure e   -> mapM_ putStrLn e >> putStrLn "failed"
         MakeSuccess _ o -> do {
              ; m_v <- dynload o ["../api"] [] "resource_dyn"
              ; makeCleaner o
              ; case m_v of
                        LoadFailure _   -> putStrLn "didn't load"
                        LoadSuccess _ v -> putStrLn $ (function v)
         }

