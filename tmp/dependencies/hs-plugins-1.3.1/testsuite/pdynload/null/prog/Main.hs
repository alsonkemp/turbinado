
import System.Plugins
-- import System.Plugins.Utils
import API

src     = "../Plugin.hs"
wrap    = "../Wrapper.hs"
apipath = "../api"

main = do status <- make src ["-i"++apipath]
          case status of
                MakeSuccess _ _ -> f
                MakeFailure e-> mapM_ putStrLn e

   where f = do v     <- load "../Plugin.o" ["../api"] [] "resource"
                --  (i,_) <- exec "ghc" ["--numeric-version"]
                --  mapM_ putStrLn i
                putStrLn "done."

