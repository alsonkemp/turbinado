{-# OPTIONS -cpp #-}

module API where

import AltData.Typeable

data Interface = Interface { 
        equals :: forall t. Eq t => t -> t -> Bool
     }

--
-- see how it hides the internal type.. but to compile GHC still checks
-- the type.
--
instance Typeable Interface where
#if __GLASGOW_HASKELL__ >= 603
    typeOf i = mkTyConApp (mkTyCon "API.Interface") []
#else
    typeOf i = mkAppTy (mkTyCon "API.Interface") []
#endif

plugin :: Interface
plugin = Interface  { equals = (==) }

