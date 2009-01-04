{-# OPTIONS -cpp -fglasgow-exts #-} 
module Poly where

import AltData.Typeable

data Fn = Fn {fn :: forall t. Eq t => t -> t -> Bool}

--
-- ignore type inside the Fn... is this correct?
--
instance Typeable Fn where
#if __GLASGOW_HASKELL__ >= 603
    typeOf _ = mkTyConApp (mkTyCon "Poly.Fn") []
#else
    typeOf _ = mkAppTy (mkTyCon "Poly.Fn") []
#endif
