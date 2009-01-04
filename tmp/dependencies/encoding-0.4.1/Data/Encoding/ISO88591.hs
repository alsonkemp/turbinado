{-# LANGUAGE CPP,TemplateHaskell,DeriveDataTypeable #-}
{- | Implements ISO\/IEC 8859-1 alias latin-1 encoding. See
     <http://en.wikipedia.org/wiki/ISO/IEC_8859-1> for further information.
 -}
module Data.Encoding.ISO88591
	(ISO88591(..)
	) where

import Data.Encoding.Base
import Data.Char(ord,chr)
import Data.Word
import Control.OldException
import Data.Typeable

data ISO88591 = ISO88591 deriving (Eq,Show,Typeable)

enc :: Char -> Word8
enc c = if ord c < 256
	then fromIntegral $ ord c
	else throwDyn (HasNoRepresentation c)

instance Encoding ISO88591 where
	encode _ = encodeSinglebyte enc
	encodeLazy _ = encodeSinglebyteLazy enc
	encodable _ c = ord c < 256
	decode _ = decodeSinglebyte (chr.fromIntegral) 
