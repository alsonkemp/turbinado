{-# LANGUAGE CPP,TemplateHaskell,DeriveDataTypeable #-}
{- | Implements ISO\/IEC 8859-2 alias latin-2 encoding. See
     <http://en.wikipedia.org/wiki/ISO/IEC_8859-2> for further informations.
 -}
module Data.Encoding.ISO88592
	(ISO88592(..)) where

import Data.Array
import Data.Map hiding ((!))
import Data.Word
import Data.Encoding.Base
import Data.ByteString hiding (length,map)
import Prelude hiding (lookup,all)
import Control.OldException
import Data.Typeable

data ISO88592 = ISO88592 deriving (Eq,Show,Typeable)

enc :: Char -> Word8
enc c = case lookup c encodeMap of
	Just v -> v
	Nothing -> throwDyn (HasNoRepresentation c)

instance Encoding ISO88592 where
	encode _ = encodeSinglebyte enc
	encodeLazy _ = encodeSinglebyteLazy enc
	encodable _ c = member c encodeMap
	decode _ = decodeSinglebyte (\w -> decodeArr!w)

decodeArr :: Array Word8 Char
#ifndef __HADDOCK__
decodeArr = $(decodingArray "8859-2.TXT")
#endif

encodeMap :: Map Char Word8
#ifndef __HADDOCK__
encodeMap = $(encodingMap "8859-2.TXT")
#endif
