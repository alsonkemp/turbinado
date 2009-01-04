{-# LANGUAGE DeriveDataTypeable #-}
{- | This module implements KOI8-U encoding which covers the ukrainian alphabet.
     See <http://en.wikipedia.org/wiki/KOI8-U> for more information.
 -}
module Data.Encoding.KOI8U
	(KOI8U(..)) where

import Control.OldException (throwDyn)
import Data.Word
import Data.Array.Unboxed
import Data.Encoding.Base
import Data.Char (chr,ord)
import Data.Map (Map,fromList,lookup,member)
import qualified Data.ByteString.Lazy as Lazy
import Prelude hiding (lookup)
import Data.Typeable

data KOI8U = KOI8U deriving (Eq,Show,Typeable)

koi8uArr :: UArray Word8 Char
koi8uArr = listArray (128,255) koi8uList

koi8uMap :: Map Char Word8
koi8uMap = fromList (zip koi8uList [0..])

koi8uList :: [Char]
koi8uList =
	['\x2500','\x2502','\x250c','\x2510','\x2514','\x2518','\x251c','\x2524'
	,'\x252c','\x2534','\x253c','\x2580','\x2584','\x2588','\x258c','\x2590'
	,'\x2591','\x2592','\x2593','\x2320','\x25a0','\x2219','\x221a','\x2248'
	,'\x2264','\x2265','\x00a0','\x2321','\x00b0','\x00b2','\x00b7','\x00f7'
	,'\x2550','\x2551','\x2552','\x0451','\x0454','\x2554','\x0456','\x0457'
	,'\x2557','\x2558','\x2559','\x255a','\x255b','\x0491','\x255d','\x255e'
	,'\x255f','\x2560','\x2561','\x0401','\x0403','\x2563','\x0406','\x0407'
	,'\x2566','\x2567','\x2568','\x2569','\x256a','\x0490','\x256c','\x00a9'
	,'\x044e','\x0430','\x0431','\x0446','\x0434','\x0435','\x0444','\x0433'
	,'\x0445','\x0438','\x0439','\x043a','\x043b','\x043c','\x043d','\x043e'
	,'\x043f','\x044f','\x0440','\x0441','\x0442','\x0443','\x0436','\x0432'
	,'\x044c','\x044b','\x0437','\x0448','\x044d','\x0449','\x0447','\x044a'
	,'\x042e','\x0410','\x0411','\x0426','\x0414','\x0415','\x0424','\x0413'
	,'\x0425','\x0418','\x0419','\x041a','\x041b','\x041c','\x041d','\x041e'
	,'\x041f','\x042f','\x0420','\x0421','\x0422','\x0423','\x0416','\x0412'
	,'\x042c','\x042b','\x0417','\x0428','\x042d','\x0429','\x0427','\x042a'
	]

koi8uDecode :: Word8 -> Char
koi8uDecode ch
	| ch < 128  = chr $ fromIntegral ch
	| otherwise = koi8uArr!ch

koi8uEncode :: Char -> Word8
koi8uEncode ch
	| ch < '\128' = fromIntegral $ ord ch
	| otherwise   = case lookup ch koi8uMap of
		Just w  -> w
		Nothing -> throwDyn (HasNoRepresentation ch)

instance Encoding KOI8U where
	encode _ = encodeSinglebyte koi8uEncode
	encodeLazy _ = encodeSinglebyteLazy koi8uEncode
	encodable _ c = (c < '\128') || (member c koi8uMap)
	decode _ = decodeSinglebyte koi8uDecode
	decodeLazy _ str = concatMap (decodeSinglebyte koi8uDecode) (Lazy.toChunks str)
	decodable _ = const True
