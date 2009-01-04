{-# LANGUAGE CPP,TemplateHaskell,DeriveDataTypeable #-}
module Data.Encoding.ISO88594
	(ISO88594(..)) where

import Data.Array ((!),Array)
import Data.Word (Word8)
import Data.Map (Map,lookup,member)
import Data.Encoding.Base
import Prelude hiding (lookup)
import Control.OldException (throwDyn)
import Data.Typeable

data ISO88594 = ISO88594 deriving (Eq,Show,Typeable)

enc :: Char -> Word8
enc c = case lookup c encodeMap of
	Just v -> v
	Nothing -> throwDyn (HasNoRepresentation c)

instance Encoding ISO88594 where
	encode _ = encodeSinglebyte enc
	encodeLazy _ = encodeSinglebyteLazy enc
	encodable _ c = member c encodeMap
	decode _ = decodeSinglebyte (decodeArr!)

decodeArr :: Array Word8 Char
#ifndef __HADDOCK__
decodeArr = $(decodingArray "8859-4.TXT")
#endif

encodeMap :: Map Char Word8
#ifndef __HADDOCK__
encodeMap =  $(encodingMap "8859-4.TXT")
#endif
