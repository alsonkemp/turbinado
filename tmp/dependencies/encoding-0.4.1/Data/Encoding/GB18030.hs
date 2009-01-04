{-# LANGUAGE CPP,DeriveDataTypeable #-}
{- | GB18030 is a chinese character encoding that is mandatory in china (if you
 -   don\'t implement it, you\'re not allowed to sell your software there).
 -}

module Data.Encoding.GB18030
	(GB18030(..))
	where

import Control.OldException
import Data.Char (chr,ord)
import Data.Word
import Data.Bits
import Data.Encoding.Base
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Typeable

#if __GLASGOW_HASKELL__>=608
import Data.ByteString.Unsafe (unsafeIndex)
#else
import Data.ByteString.Base (unsafeIndex)
#endif

import Data.Encoding.GB18030Data

data GB18030 = GB18030 deriving (Eq,Show,Typeable)

instance Encoding GB18030 where
	encode _ = encodeMultibyte encodeGB
	encodeLazy _ = encodeMultibyteLazy encodeGB
	decode _ = decodeMultibyte decodeGB
	decodeLazy _ = decodeMultibyteLazy decodeGB
	encodable _ ch = ch <= '\x10FFFF'
	decodable _ = checkValidity

data DecodingState
	= Valid
	| Invalid
	| Second
	| Third
	| Fourth
	deriving Eq

checkValidity :: ByteString -> Bool
checkValidity bs = BS.foldl' (\st w -> case st of
	Invalid			-> Invalid
	Valid	| w<=0x80	-> Valid
		| w<=0xFE	-> Second
		| otherwise	-> Invalid
	Second	| w< 0x30	-> Invalid
		| w<=0x39	-> Third
		| w<=0x7E	-> Valid
		| w==0x7F	-> Invalid
		| w<=0xFE	-> Valid
		| otherwise	-> Invalid
	Third	| w< 0x81	-> Invalid
		| w<=0xFE	-> Fourth
		| otherwise	-> Invalid
	Fourth	| w< 0x30	-> Invalid
		| w<=0x39	-> Valid
		| otherwise	-> Invalid
	) Valid bs == Valid

{- How this works: The nested if-structures form an binary tree over the
 - encoding range.
 -}
encodeGB :: Char -> (Word8,EncodeState)
encodeGB ch = if ch<='\x4946'					-- 1
	then (if ch<='\x4055'					-- 2
		then (if ch<='\x2E80'				-- 3
			then (if ch<='\x200F'			-- 4
				then (if ch<'\x0452'
					then arr 0x0000 arr1
					else range range1)
				else (if ch<'\x2643'
					then arr 0x2010 arr2
					else range range2))
			else (if ch<='\x3917'			-- 4
				then (if ch<'\x361B'
					then arr 0x2E81 arr3
					else range range3)
				else (if ch<'\x3CE1'
					then arr 0x3918 arr4
					else range range4)))
		else (if ch<='\x464B'				-- 3
			then (if ch<='\x4336'			-- 4
				then (if ch<'\x4160'
					then arr 0x4056 arr5
					else range range5)
				else (if ch<'\x44D7'
					then arr 0x4337 arr6
					else range range6))
			else (if ch<'\x478E'
				then arr 0x464C arr7
				else range range7)))
	else (if ch<='\xF92B'					-- 2
		then (if ch<='\xD7FF'				-- 3
			then (if ch<='\x4C76'			-- 4
				then (if ch<'\x49B8'
					then arr 0x4947 arr8
					else range range8)
				else (if ch<'\x9FA6'
					then arr 0x4C77 arr9
					else range range9))
			else (if ch<'\xE865'
				then arr 0xD800 arr10
				else range range10))
		else (if ch<='\xFFFF'				-- 3
			then (if ch<='\xFE2F'			-- 4
				then (if ch<'\xFA2A'
					then arr 0xF92C arr11
					else range range11)
				else (if ch<'\xFFE6'
					then arr 0xFE30 arr12
					else range range12))
			else (if ch<='\x10FFFF'			-- 4
				then range range13
				else throwDyn (HasNoRepresentation ch))))
	where
	range r = let
		(w1,w2,w3,w4) = delinear (ord ch + r)
		in (w1,Put3 w2 w3 w4)
	arr off a = let
		ind = (ord ch - off)*5
		w1 = unsafeIndex a (ind+1)
		w2 = unsafeIndex a (ind+2)
		w3 = unsafeIndex a (ind+3)
		w4 = unsafeIndex a (ind+4)
		in (w1,case unsafeIndex a ind of
			1 -> Done
			2 -> Put1 w2
			3 -> Put2 w2 w3
			4 -> Put3 w2 w3 w4)

linear :: Word8 -> Word8 -> Word8 -> Word8 -> Int
linear w1 w2 w3 w4
	= (fromIntegral (w4-0x30))
	+ (fromIntegral (w3-0x81))*10
	+ (fromIntegral (w2-0x30))*1260
	+ (fromIntegral (w1-0x81))*12600

linear2 :: Word8 -> Word8 -> Int
linear2 w1 w2 = (fromIntegral (w2 - (if w2<=0x7E
	then 0x40
	else 0x41)))
	+ (fromIntegral (w1-0x81))*190

delinear :: Int -> (Word8,Word8,Word8,Word8)
delinear n = let
	(w1,n1) = n  `divMod` 12600
	(w2,n2) = n1 `divMod`  1260
	(w3,n3) = n2 `divMod`    10
	w4      = n3
	in (fromIntegral w1+0x81
	   ,fromIntegral w2+0x30
	   ,fromIntegral w3+0x81
	   ,fromIntegral w4+0x30)

decodeGB :: [Word8] -> (Char,[Word8])
decodeGB (w1:rst)
	| w1 <=0x80	= (chr $ fromIntegral w1,rst)	-- it's ascii
	| w1 <=0xFE	= case rst of
		w2:rst2
			| w2 < 0x30	-> throwDyn (IllegalCharacter w2)
			| w2 <=0x39	-> case rst2 of
				w3:rst3
					| w3 < 0x81	-> throwDyn (IllegalCharacter w3)
					| w3 <=0xFE	-> case rst3 of
						w4:rst4
							| w4 < 0x30	-> throwDyn (IllegalCharacter w4)
							| w4 <=0x39	-> let
								v = linear w1 w2 w3 w4
								in (decodeGBFour v,rst4)
							| otherwise	-> throwDyn (IllegalCharacter w4)
						[] -> throwDyn UnexpectedEnd
					| otherwise	-> throwDyn (IllegalCharacter w3)
				[] -> throwDyn UnexpectedEnd
			| w2 <=0x7E	-> (decodeGBTwo (linear2 w1 w2),rst2)
			| w2 ==0x7F	-> throwDyn (IllegalCharacter w2)
			| w2 <=0xFE	-> (decodeGBTwo (linear2 w1 w2),rst2)
			| otherwise	-> throwDyn (IllegalCharacter w2)
		[] -> throwDyn UnexpectedEnd
	| otherwise	= throwDyn (IllegalCharacter w1)

decodeGBTwo :: Int -> Char
decodeGBTwo n = let
	rn = n*2
	w1 = unsafeIndex rrarr rn
	w2 = unsafeIndex rrarr (rn+1)
	in chr $ ((fromIntegral w1) `shiftL` 8) .|. (fromIntegral w2)

decodeGBFour :: Int -> Char
decodeGBFour v = if v<=17858				-- 1
	then (if v<=15582					-- 2
		then (if v<=11328				-- 3
			then (if v<=7921			-- 4
				then (if v<820
					then arr 0 rarr1
					else range range1)
				else (if v<9219
					then arr 7922 rarr2
					else range range2))
			else (if v<=13737			-- 4
				then (if v<12973
					then arr 11329 rarr3
					else range range3)
				else (if v<14698
					then arr 13738 rarr4
					else range range4)))
		else (if v<=17101				-- 3
			then (if v<=16317			-- 4
				then (if v<15847
					then arr 15583 rarr5
					else range range5)
				else (if v<16729
					then arr 16318 rarr6
					else range range6))
			else (if v<17418
				then arr 17102 rarr7
				else range range7)))
	else (if v<=37844					-- 2
		then (if v<=33468				-- 3
			then (if v<=18663			-- 4
				then (if v<17961
					then arr 17859 rarr8
					else range range8)
				else (if v<19043
					then arr 18664 rarr9
					else range range9))
			else (if v<33550
				then arr 33469 rarr10
				else range range10))
		else (if v<=39419				-- 3
			then (if v<=39107			-- 4
				then (if v<38078
					then arr 37845 rarr11
					else range range11)
				else (if v<39394
					then arr 39108 rarr12
					else range range12))
			else (if v<=1237575 && v>=189000
				then range range13
				else throwDyn OutOfRange)))
	where
	arr off a = let
		v' = (v-off)*2
		w1 = unsafeIndex a v'
		w2 = unsafeIndex a (v'+1)
		in chr $ ((fromIntegral w1) `shiftL` 8)
		      .|. (fromIntegral w2)
	range r = chr (v-r)

range1,range2,range3,range4,range5,range6,range7,range8,range9,range10,range11,range12,range13 :: Int
range1 = -286
range2 = -576
range3 = -878
range4 = -887
range5 = -889
range6 = -894
range7 = -900
range8 = -911
range9 = -21827
range10 = -25943
range11 = -25964
range12 = -26116
range13 = 123464
