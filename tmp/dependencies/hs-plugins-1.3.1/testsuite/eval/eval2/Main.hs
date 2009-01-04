import System.Eval.Haskell

main = do m_s <- eval "map toUpper \"haskell\"" ["Data.Char"]
          case m_s of
                Nothing -> putStrLn "typechecking failed"
                Just s  -> putStrLn s
