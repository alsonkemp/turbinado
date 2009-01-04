
import System.Eval.Haskell

main = do i <- eval "1 + 6 :: Int" [] :: IO (Maybe Int)
          if isJust i then putStrLn $ show (fromJust i) else return ()
