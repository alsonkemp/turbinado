import System.Plugins
import API
import System.Directory

main = do
#if __GLASGOW_HASKELL__ >= 604
        tmpDir <- getTemporaryDirectory
#else
        let tmpDir = "/tmp"
#endif
        status <- make "../Plugin.hs" [ "-i../api", "-odir", tmpDir ]
        o <- case status of
                MakeSuccess _ o -> return o
                MakeFailure e -> mapM_ putStrLn e >> error "didn't compile"
        m_v     <- load o ["../api"] [] "resource"
        v <- case m_v of
            LoadSuccess _ v -> return v
            _               -> error "load failed"
        putStrLn $ field v 
        mapM_ removeFile [(tmpDir ++ "/Plugin.hi"), (tmpDir ++ "/Plugin.o") ]

