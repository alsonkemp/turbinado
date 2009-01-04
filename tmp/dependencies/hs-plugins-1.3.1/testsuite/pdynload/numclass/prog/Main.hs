
import System.Plugins
import API

src     = "../Plugin.hs"
wrap    = "../Wrapper.hs"
apipath = "../api"

main = do status <- make src ["-i"++apipath]
          case status of
                MakeFailure _   -> putStrLn "make failed"
                MakeSuccess _ _ -> do {

       ;v <- pdynload "../Plugin.o" ["../api"] [] "API.Interface Integer" "resource"
       ;case v of
          LoadSuccess _ a -> let D i = snd a in putStrLn $ show i
          _               -> putStrLn "wrong types"

       }
