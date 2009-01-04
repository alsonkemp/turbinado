{-# OPTIONS -fglasgow-exts #-}
--
-- polymorphic eval!
--

module Main where

import Poly
import System.Eval.Haskell

main = do m_f <- eval "Fn (\\x y -> x == y)" ["Poly"]
          when (isJust m_f) $ do
                let (Fn f) = fromJust m_f
                putStrLn $ show (f True True)
                putStrLn $ show (f 1 2)
