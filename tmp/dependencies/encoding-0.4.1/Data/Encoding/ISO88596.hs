{-# LANGUAGE CPP,TemplateHaskell,DeriveDataTypeable #-}
module Data.Encoding.ISO88596
	(ISO88596(..)) where

import Data.Array ((!),Array)
import Data.Word (Word8)
import Data.Map (Map,lookup,member)
import Data.Encoding.Base
import Prelude hiding (lookup)
import Control.OldException (throwDyn)
import Data.Typeable

data ISO88596 = ISO88596 deriving (Eq,Show,Typeable)

instance Encoding ISO88596 where
	encode _ = encodeSinglebyte (\c -> case lookup c encodeMap of
		Just v -> v
		Nothing -> throwDyn (HasNoRepresentation c))
	encodable _ c = member c encodeMap
	decode _ = decodeSinglebyte (decodeArr!)

decodeArr :: Array Word8 Char
#ifndef __HADDOCK__
decodeArr = $(decodingArray "8859-6.TXT")
#endif

encodeMap :: Map Char Word8
#ifndef __HADDOCK__
encodeMap =  $(encodingMap "8859-6.TXT")
#endif
