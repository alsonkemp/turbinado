
import System.Plugins.Make
import System.Eval.Haskell

main = do make "a/Extra.hs" []
        
          i <- unsafeEval_ "show (Just (1 + 6 :: Int)) ++ extra" 
                        ["Data.Maybe", "Extra"] 
                        ["-ia"]      -- no make flags
                        []           -- no package.confs
                        ["a"]        -- include paths to load from
                        :: IO (Either [String] String)
               
          case i of
                Right i -> putStrLn $ show i
                Left es -> mapM_ putStrLn es
