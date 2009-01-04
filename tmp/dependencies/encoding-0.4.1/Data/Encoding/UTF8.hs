{-# LANGUAGE DeriveDataTypeable #-}
{- | This module implements UTF-8 encoding and decoding as in RFC 3629.
     See <http://en.wikipedia.org/wiki/UTF-8> for more information.
 -}
module Data.Encoding.UTF8
	(UTF8(..)) where

import Data.Bits
import Data.Char (ord,chr)
import Data.Encoding.Base
import Data.ByteString
import Data.Word
import Prelude hiding (length)
import Control.OldException
import Data.Typeable

data UTF8
	= UTF8		-- ^ Very forgiving decoding mechanism, accepts everything that it can make any sense of.
	| UTF8Strict	-- ^ More strict decoding, doesn\'t accept sequences that have a too long representation and checks bits that aren\'t used in the decoding.
	deriving (Eq,Show,Typeable)

encodeUTF8 :: Char -> (Word8,EncodeState)
encodeUTF8 x
	| n<=0x0000007F = (v,Done)
	| n<=0x000007FF = (fromIntegral $ 0xC0 .|. (n `shiftR` 6)
		,Put1 (0x80 .|. (v .&. 0x3F)))
	| n<=0x0000FFFF = (fromIntegral $ 0xE0 .|. (n `shiftR` 12)
		,Put2 (fromIntegral $
			0x80 .|. ((n `shiftR` 6) .&. 0x3F))
			(fromIntegral $
			0x80 .|. (n .&. 0x3F)))
	| n<=0x0010FFFF = (fromIntegral $ 0xF0 .|. (n `shiftR` 18)
		,Put3 (fromIntegral $
			0x80 .|. ((n `shiftR` 12) .&. 0x3F))
			(fromIntegral $
			0x80 .|. ((n `shiftR`  6) .&. 0x3F))
			(fromIntegral $
			0x80 .|. (n .&. 0x3F)))
	| otherwise = throwDyn (HasNoRepresentation x)
	where
	n = ord x
	v = fromIntegral $ ord x

decodeUTF8 :: [Word8] -> (Char,[Word8])
decodeUTF8 ~(w1:rest1)
	| w1<=0x7F = (chr $ fromIntegral w1,rest1)
	| w1<=0xBF = throwDyn (IllegalCharacter w1)
	| w1<=0xDF = case rest1 of
		(w2:rest2) -> (chr $ ((fromIntegral $ w1 .&. 0x1F) `shiftL` 6)
			.|. (fromIntegral (w2 .&. 0x3F)),rest2)
		_ -> throwDyn UnexpectedEnd
	| w1<=0xEF = case rest1 of
		(w2:w3:rest3) -> (chr $ ((fromIntegral $ w1 .&. 0x0F) `shiftL` 12)
			.|. ((fromIntegral $ w2 .&. 0x3F) `shiftL` 6)
			.|. (fromIntegral $ w3 .&. 0x3F),rest3)
		_ -> throwDyn UnexpectedEnd
	| w1<=0xF7 = case rest1 of
		(w2:w3:w4:rest4) -> (chr $ ((fromIntegral $ w1 .&. 0x07) `shiftL` 18)
			.|. ((fromIntegral $ w2 .&. 0x3F) `shiftL` 12)
			.|. ((fromIntegral $ w3 .&. 0x3F) `shiftL`  6)
			.|. (fromIntegral $ w4 .&. 0x3F),rest4)
		_ -> throwDyn UnexpectedEnd
	| otherwise = throwDyn (IllegalCharacter w1)

decodeUTF8Strict :: [Word8] -> (Char,[Word8])
decodeUTF8Strict ~(w1:rest1)
	| w1<=0x7F = (chr $ fromIntegral w1,rest1)
	| w1<=0xBF = throwDyn (IllegalCharacter w1)
	| w1<=0xDF = case rest1 of
		(w2:rest2)
			| invalidExtend w2 -> throwDyn (IllegalCharacter w2)
			| otherwise -> let
				v1 = w1 .&. 0x1F
				in if v1 <= 1
					then throwDyn (IllegalRepresentation [w1,w2])
					else (chr $ ((fromIntegral v1) `shiftL` 6)
						.|. (fromIntegral (w2 .&. 0x3F)),rest2)
		_ -> throwDyn UnexpectedEnd
	| w1<=0xEF = case rest1 of
		(w2:w3:rest3)
			| invalidExtend w2 -> throwDyn (IllegalCharacter w2)
			| invalidExtend w3 -> throwDyn (IllegalCharacter w3)
			| otherwise -> let
				v1 = w1 .&. 0x0F
				v2 = w2 .&. 0x3F
				in if v1 == 0 && v2 < 0x20
					then throwDyn (IllegalRepresentation [w1,w2,w3])
					else (chr $ ((fromIntegral v1) `shiftL` 12)
						.|. ((fromIntegral v2) `shiftL` 6)
						.|. (fromIntegral $ w3 .&. 0x3F),rest3)
		_ -> throwDyn UnexpectedEnd
	| w1<=0xF7 = case rest1 of
		(w2:w3:w4:rest4)
			| invalidExtend w2 -> throwDyn (IllegalCharacter w2)
			| invalidExtend w3 -> throwDyn (IllegalCharacter w3)
			| invalidExtend w4 -> throwDyn (IllegalCharacter w4)
			| otherwise -> let
				v1 = w1 .&. 0x07
				v2 = w2 .&. 0x3F
				in if v1 == 0 && v2 < 0x10
					then throwDyn (IllegalRepresentation [w1,w2,w3,w4])
					else (chr $ ((fromIntegral $ w1 .&. 0x07) `shiftL` 18)
						.|. ((fromIntegral $ w2 .&. 0x3F) `shiftL` 12)
						.|. ((fromIntegral $ w3 .&. 0x3F) `shiftL`  6)
						.|. (fromIntegral $ w4 .&. 0x3F),rest4)
		_ -> throwDyn UnexpectedEnd
	| otherwise = throwDyn (IllegalCharacter w1)
	where
	invalidExtend wrd = wrd .&. 0xC0 /= 0x80

data UTF8AnalyzeState
	= Skip !Int
	| CheckAndSkip !Word8 !Int
	| Ok
	| Failed
	deriving Eq

instance Encoding UTF8 where
	encode _ = encodeMultibyte encodeUTF8
	encodeLazy _ = encodeMultibyteLazy encodeUTF8
	encodable _ c = ord c <= 0x0010FFFF
	decode UTF8 = decodeMultibyte decodeUTF8
	decode UTF8Strict = decodeMultibyte decodeUTF8Strict
	decodeLazy UTF8 = decodeMultibyteLazy decodeUTF8
	decodeLazy UTF8Strict = decodeMultibyteLazy decodeUTF8Strict
	decodable UTF8 str = (foldl' (\st w -> case st of
		Ok	| w<=0x7F -> Ok
			| w<=0xBF -> Failed
			| w<=0xDF -> Skip 0
			| w<=0xEF -> Skip 1
			| w<=0xF7 -> Skip 2
			| otherwise -> Failed
		Failed -> Failed
		Skip n -> if w .&. 0xC0 == 0x80
			then (if n == 0 then Ok else Skip (n-1))
			else Failed) Ok str) == Ok
	decodable UTF8Strict str = (foldl' (\st w -> case st of
		Ok	| w<=0x7F -> Ok
			| w<=0xBF -> Failed
			| w<=0xDF -> if w .&. 0x1F <= 1
				then Failed
				else Skip 0
			| w<=0xEF -> if w .&. 0x0F == 0
				then CheckAndSkip 0x20 1
				else Skip 1
			| w<=0xF7 -> if w .&. 0x07 == 0
				then CheckAndSkip 0x10 2
				else Skip 2
			| otherwise -> Failed
		Failed -> Failed
		Skip n -> if w .&. 0xC0 == 0x80
			then (if n == 0 then Ok else Skip (n-1))
			else Failed
		CheckAndSkip chk n -> if w .&. 0xC0 == 0x80 && w .&. 0x3F >= chk
			then (if n == 0 then Ok else Skip (n-1))
			else Failed
			) Ok str) == Ok
