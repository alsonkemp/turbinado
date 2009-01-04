--
-- lambda abstraction!
--
--
-- needs unsafeEval because eval has a broken Dynamic check
--
import System.Eval.Haskell

main = do fn <- unsafeEval "(\\x -> (x,x::Int))" [] :: IO (Maybe (Int -> (Int,Int)))
          when (isJust fn) $ putStrLn $ show $ (fromJust fn) 7
