{-# LANGUAGE CPP,TemplateHaskell,DeriveDataTypeable #-}
{- | Implements ISO 8859-3 encoding, alias latin-3, alias south european
 -}
module Data.Encoding.ISO88593
	(ISO88593(..)) where

import Data.Array ((!),Array)
import Data.Word (Word8)
import Data.Map (Map,lookup,member)
import Data.Encoding.Base
import Prelude hiding (lookup)
import Control.OldException (throwDyn)
import Data.Typeable

data ISO88593 = ISO88593 deriving (Eq,Show,Typeable)

enc :: Char -> Word8
enc c = case lookup c encodeMap of
	Just v -> v
	Nothing -> throwDyn (HasNoRepresentation c)

instance Encoding ISO88593 where
	encode _ = encodeSinglebyte enc
	encodeLazy _ = encodeSinglebyteLazy enc
	encodable _ c = member c encodeMap
	decode _ = decodeSinglebyte (decodeArr!)

decodeArr :: Array Word8 Char
#ifndef __HADDOCK__
decodeArr = $(decodingArray "8859-3.TXT")
#endif

encodeMap :: Map Char Word8
#ifndef __HADDOCK__
encodeMap =  $(encodingMap "8859-3.TXT")
#endif
