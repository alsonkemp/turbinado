{-# LANGUAGE DeriveDataTypeable #-}
{- | This module implements UTF-16 encoding and decoding as in RFC 2781.
     See <http://en.wikipedia.org/wiki/UTF-16> for more information.
 -}
module Data.Encoding.UTF16
	(UTF16(..)
	) where

import Data.Encoding.Base
import Data.Char(ord,chr)
import Data.Bits
import Data.Int
import Data.Word
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Prelude hiding (length)
import Control.OldException
import Data.Dynamic (toDyn)
import Data.Typeable

data UTF16
	= UTF16		-- ^ Decodes big and little endian, encodes big endian.
	| UTF16BE	-- ^ Big endian decoding and encoding, fails if the string isn\'t actually big endian.
	| UTF16LE	-- ^ Little endian decoding and encoding.
	deriving (Eq,Show,Typeable)

utf16enc :: Bool -> (EncodeState,String) -> Maybe (Word8,(EncodeState,String))
utf16enc _ (Done,[])   = Nothing
utf16enc True (Done,x:xs)
	| n<=0x0000FFFF = Just
		(fromIntegral $ n `shiftR` 8
		,(Put1 (fromIntegral $ n),xs))
	| n<=0x0010FFFF = Just
		(fromIntegral $ 0xD8 .|. (n' `shiftR` 18)
		,(Put3 (fromIntegral $ (n' `shiftR` 10))
			(fromIntegral $
			0xDC .|. ((n' `shiftR` 8) .&. 0x03))
			(fromIntegral n'),xs))
	| otherwise = throwDyn $ HasNoRepresentation x
	where
	n  = ord x
	n' = n - 0x10000
utf16enc False (Done,x:xs)
	| n<=0x0000FFFF = Just
		(fromIntegral $ n
		,(Put1 (fromIntegral $ n `shiftR` 8),xs))
	| n<=0x0010FFFF = Just
		(fromIntegral n'
		,(Put3 (fromIntegral $
			0xDC .|. ((n' `shiftR` 8) .&. 0x03))
			(fromIntegral $ (n' `shiftR` 10))
			(fromIntegral $ 0xD8 .|. (n' `shiftR` 18)),xs))
	| otherwise = throwDyn $ HasNoRepresentation x
	where
	n  = ord x
	n' = n - 0x10000
utf16enc _ (Put3 w1 w2 w3,xs) = Just (w1,(Put2 w2 w3,xs))
utf16enc _ (Put2 w1 w2,xs) = Just (w1,(Put1 w2,xs))
utf16enc _ (Put1 w1,xs) = Just (w1,(Done,xs))

{-# SPECIALIZE utf16dec :: Bool -> Word8 -> Word8 -> Word8 -> Word8 -> (Char,Int) #-}
{-# SPECIALIZE utf16dec :: Bool -> Word8 -> Word8 -> Word8 -> Word8 -> (Char,Int64) #-}
utf16dec :: Num a => Bool -> Word8 -> Word8 -> Word8 -> Word8 -> (Char,a)
utf16dec be s1 s2 s3 s4
	| w1< 0xD8 || w1> 0xDF
		= (chr $ ((fromIntegral w1) `shiftL` 8) .|. (fromIntegral w2),2)
	| w1> 0xDB = throwDyn $ IllegalCharacter w1
	| w3< 0xDC || w3>0xDF = throwDyn $ IllegalCharacter w3
	| otherwise = (chr $ (((fromIntegral w1 .&. 0x03) `shiftL` 18)
		.|. ((fromIntegral w2) `shiftL` 10)
		.|. ((fromIntegral w3 .&. 0x03) `shiftL` 8)
		.|. (fromIntegral w4)) + 0x10000,4)
	where
	(w1,w2,w3,w4) = if be then (s1,s2,s3,s4) else (s2,s1,s4,s3)

instance Encoding UTF16 where
	encode enc str = unfoldr (utf16enc (enc/=UTF16LE)) (case enc of
		UTF16 -> Put2 0xFE 0xFF
		_ -> Done,str)
	encodeLazy enc str = LBS.unfoldr (utf16enc (enc/=UTF16LE)) (case enc of
		UTF16 -> Put2 0xFE 0xFF
		_ -> Done,str)
	encodable _ c = ord c <= 0x0010FFFF
	decode bo str = case findByteOrder str of
		Nothing -> decode' (bo/=UTF16LE) 0
		Just big -> decode' big 2
		where
		l = BS.length str
		decode' be i = if i>=l
			then []
			else c:decode' be (i+took)
			where
			(c,took) = mapException (\ex -> case ex of
				ErrorCall _ -> DynException (toDyn UnexpectedEnd)
				_ -> ex) (utf16dec be s1 s2 s3 s4)
			s1 = index str i
			s2 = index str (i+1)
			s3 = index str (i+2)
			s4 = index str (i+3)
	decodeLazy bo str = case findByteOrderLazy str of
		Nothing -> decode' (bo/=UTF16LE) 0
		Just big -> decode' big 2
		where
		l = LBS.length str
		decode' be i = if i>=l
			then []
			else c:decode' be (i+took)
			where
			(c,took) = mapException (\ex -> case ex of
				ErrorCall _ -> DynException (toDyn UnexpectedEnd)
				_ -> ex) (utf16dec be s1 s2 s3 s4)
			s1 = LBS.index str i
			s2 = LBS.index str (i+1)
			s3 = LBS.index str (i+2)
			s4 = LBS.index str (i+3)
	decodable bo str = case findByteOrder str of
		Nothing  -> check' (bo/=UTF16LE) (length str) 0
		Just big -> check' big  (length str) 2
		where
		check' be m i
			| m == i   = True
			| m == i+1 = False
			| w1< 0xD8 || w1> 0xDF = check' be m (i+2)
			| w1> 0xDB = False
			| m <= i+3 = False
			| w3< 0xDC || w3>0xDF = False
			| otherwise = check' be m (i+4)
			where
			(w1,w3) = if be then (s1,s3) else (s2,s4)
			s1 = index str i
			s2 = index str (i+1)
			s3 = index str (i+2)
			s4 = index str (i+3)

findByteOrder :: ByteString -> Maybe Bool
findByteOrder str
	| length str < 2 = Nothing
	| w1 == 0xFE && w2 == 0xFF = Just True
	| w1 == 0xFF && w2 == 0xFE = Just False
	| otherwise = Nothing
	where
	w1 = index str 0
	w2 = index str 1


findByteOrderLazy :: LBS.ByteString -> Maybe Bool
findByteOrderLazy str = case LBS.unpack (LBS.take 2 str) of
	[w1,w2]
		| w1 == 0xFE && w2 == 0xFF -> Just True
		| w1 == 0xFF && w2 == 0xFE -> Just False
		| otherwise -> Nothing
	_ -> Nothing
