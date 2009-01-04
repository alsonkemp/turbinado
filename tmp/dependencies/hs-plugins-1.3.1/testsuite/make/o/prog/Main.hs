import System.Plugins
import API

import System.Directory

-- note: the name of the original *source* module is used to find
-- symbols in the *object* file. load works out what the source file
-- name was by looking at the object file name, i.e. it assumes they
-- have the same name. so, if you are going to store objects in a
-- tmpdir, you should make a tmp directory, and store them inside that,
-- rather than mkstemp'ing the name of the object file yourself.
--
-- this should go away once we can read .hi files.

main = do
#if __GLASGOW_HASKELL__ >= 604
        tmpDir <- getTemporaryDirectory
#else
        let tmpDir = "/tmp"
#endif
        make "../Plugin.hs" [ "-i../api", "-o", (tmpDir ++ "/Plugin.o") ]
        m_v   <- load (tmpDir ++ "/Plugin.o") ["../api"] [] "resource"
        v <- case m_v of
            LoadSuccess _ v -> return v
            _               -> error "load failed"
        putStrLn $ field v 

        mapM_ removeFile [ (tmpDir ++ "/Plugin.hi"), (tmpDir ++ "/Plugin.o") ]
