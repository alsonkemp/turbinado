{-# LANGUAGE DeriveDataTypeable #-}
{- | This module implements UTF-32 encoding and decoding.
     See <http://en.wikipedia.org/wiki/UTF-32> for more information.
 -}
module Data.Encoding.UTF32
	(UTF32(..))
	where

import Data.Bits
import Data.Char (ord,chr)
import Data.Encoding.Base
import Data.Word
import Control.OldException (throwDyn)
import Data.Typeable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

data UTF32
	= UTF32		-- ^ Detects big or little endian through the use of the BOM (Byte Order Mask) character. Defaults to big endian if not present.
	| UTF32BE	-- ^ Encodes and decodes using the big endian encoding.
	| UTF32LE	-- ^ Encodes and decodes using the little endian encoding.
	deriving (Eq,Show,Typeable)

bom :: Char
bom = '\xFEFF'

instance Encoding UTF32 where
	encode UTF32   str = encodeMultibyte encodeUTF32be (bom:str)
	encode UTF32LE str = encodeMultibyte encodeUTF32le str
	encode UTF32BE str = encodeMultibyte encodeUTF32be str
	encodeLazy UTF32 str   = encodeMultibyteLazy encodeUTF32be (bom:str)
	encodeLazy UTF32LE str = encodeMultibyteLazy encodeUTF32le str
	encodeLazy UTF32BE str = encodeMultibyteLazy encodeUTF32be str
	encodable _ c = ord c < 0x0010FFFF
	decode UTF32 str   = let
		(start,rest) = BS.splitAt 4 str
		in case BS.unpack start of
			[0x00,0x00,0xFE,0xFF] -> decode UTF32BE rest
			[0xFE,0xFF,0x00,0x00] -> decode UTF32LE rest
			_                     -> decode UTF32BE str
	decode UTF32LE str = decodeMultibyte decodeUTF32le str
	decode UTF32BE str = decodeMultibyte decodeUTF32be str
	decodeLazy UTF32 str   = let
		(start,rest) = LBS.splitAt 4 str
		in case LBS.unpack start of
			[0x00,0x00,0xFE,0xFF] -> decodeLazy UTF32BE rest
			[0xFE,0xFF,0x00,0x00] -> decodeLazy UTF32LE rest
			_                     -> decodeLazy UTF32BE str
	decodeLazy UTF32LE str = decodeMultibyteLazy decodeUTF32le str
	decodeLazy UTF32BE str = decodeMultibyteLazy decodeUTF32be str

encodeUTF32be :: Char -> (Word8,EncodeState)
encodeUTF32be ch = let
	w  = ord ch	
	w1 = fromIntegral $ w `shiftR` 24
	w2 = fromIntegral $ w `shiftR` 16
	w3 = fromIntegral $ w `shiftR`  8
	w4 = fromIntegral $ w
	in (w1,Put3 w2 w3 w4)

encodeUTF32le :: Char -> (Word8,EncodeState)
encodeUTF32le ch = let
	w  = ord ch	
	w1 = fromIntegral $ w `shiftR` 24
	w2 = fromIntegral $ w `shiftR` 16
	w3 = fromIntegral $ w `shiftR`  8
	w4 = fromIntegral $ w
	in (w4,Put3 w3 w2 w1)

decodeUTF32be :: [Word8] -> (Char,[Word8])
decodeUTF32be (w1:w2:w3:w4:rest) = let
	v = (fromIntegral w1 `shiftL` 24) .|.
	    (fromIntegral w2 `shiftL` 16) .|.
	    (fromIntegral w3 `shiftL`  8) .|.
	    (fromIntegral w4)
	in if v < 0x0010FFFF
		then (chr v,rest)
		else throwDyn (IllegalRepresentation [w1,w2,w3,w4])
decodeUTF32be _ = throwDyn UnexpectedEnd
	
decodeUTF32le :: [Word8] -> (Char,[Word8])
decodeUTF32le (w1:w2:w3:w4:rest) = let
	v = (fromIntegral w4 `shiftL` 24) .|.
	    (fromIntegral w3 `shiftL` 16) .|.
	    (fromIntegral w2 `shiftL`  8) .|.
	    (fromIntegral w1)
	in if v < 0x0010FFFF
		then (chr v,rest)
		else throwDyn (IllegalRepresentation [w1,w2,w3,w4])
decodeUTF32le _ = throwDyn UnexpectedEnd
