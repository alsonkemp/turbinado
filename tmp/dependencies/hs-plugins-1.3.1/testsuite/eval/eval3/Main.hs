{-# OPTIONS -cpp #-}
--
-- Should evaluate to '3', unless something goes wrong.
--
-- Not so bad to use AltData, as it is already derived for all the basic
-- types. Then, just replace deriving Typeable, with hand-derived
-- instance of Typeable (see hs-plugins/testsuite/eval/eval_fn1/Poly.hs
--
--

#include "../../../config.h"

import System.Eval
import Data.Dynamic

main = do
    a <- return $ toDyn (3::Integer)

    -- so, we try to compile a function that takes a dyn.
    -- looks like with GHC 6.4, we need to make sure the package.confs work:
    m_b <- unsafeEval_ "\\dyn -> fromDyn dyn (7 :: Integer)"
                ["Data.Dynamic"]
                [ ]
                [ ]
                []

    case m_b of 
        Left s   -> mapM_ putStrLn s
        Right b  -> putStrLn $ show (b a :: Integer) -- now apply it
                
{-
-- should work, but doesn't. type check fails 
-- (due to static vs dynamic typing issue)

     m_b <- unsafeEval_ "\\dyn -> fromMaybe (7 :: Int) (fromDynamic dyn)"
                                ["Data.Dynamic","Data.Maybe"] [] []
-}

