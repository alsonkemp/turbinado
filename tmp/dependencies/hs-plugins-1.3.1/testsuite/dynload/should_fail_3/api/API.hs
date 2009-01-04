{-# OPTIONS -cpp -fglasgow-exts #-}

module API where

import AltData.Typeable
import GHC.Base

data Interface = Interface { 
        function :: String
   }

instance Typeable Interface where
#if __GLASGOW_HASKELL__ >= 603
    typeOf _ = mkTyConApp (mkTyCon "API.Interface") []
#else
    typeOf _ = mkAppTy (mkTyCon "API.Interface") []
#endif

plugin :: Interface
plugin = Interface  { function = "goodbye" }

unsafeCoerce = unsafeCoerce#
