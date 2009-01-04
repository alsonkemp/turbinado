{-# LANGUAGE CPP,TemplateHaskell,DeriveDataTypeable #-}

module Data.Encoding.Base
	(Encoding(..)
	,EncodeState(..)
	,encodeMultibyte
	,encodeMultibyteLazy
	,decodeMultibyte
	,decodeMultibyteLazy
	,encodeSinglebyte
	,encodeSinglebyteLazy
	,decodeSinglebyte
	,EncodingException(..)
	,DecodingException(..)
	,decodingArray
	,encodingMap)
	where

import Data.Array(array)
import Data.ByteString (ByteString,unfoldrN,unfoldr,length,index,unpack)
import qualified Data.ByteString.Lazy as LBS
import Data.Encoding.Helper.Template

#if __GLASGOW_HASKELL__>=608
import Data.ByteString.Unsafe(unsafeIndex)
#else
import Data.ByteString.Base(unsafeIndex)
#endif

import Data.Map (Map,fromList,lookup)
import Data.Char(chr)
import Data.Maybe(mapMaybe)
import Data.Typeable
import Data.Word
import Prelude hiding (lookup,length)
import qualified Prelude
import Control.OldException
import Data.Dynamic(toDyn)

import Language.Haskell.TH

{- | Represents an encoding, supporting various methods of de- and encoding.
     Minimal complete definition: encode, decode
 -}
class Encoding enc where
	-- | Encode a 'String' into a strict 'ByteString'. Throws the
	--   'HasNoRepresentation'-Exception if it encounters an unrepresentable
	--   character.
	encode :: enc -> String -> ByteString
	-- | Encode a 'String' into a lazy 'Data.ByteString.Lazy.ByteString'.
	encodeLazy :: enc -> String -> LBS.ByteString
	encodeLazy e str = LBS.fromChunks [encode e str]
	-- | Whether or not the given 'Char' is representable in this encoding. Default: 'True'.
	encodable :: enc -> Char -> Bool
	encodable _ _ = True
	-- | Decode a strict 'ByteString' into a 'String'. If the string is not
	--   decodable, a 'DecodingException' is thrown.
	decode :: enc -> ByteString -> String
	decodeLazy :: enc -> LBS.ByteString -> String
	decodeLazy e str = concatMap (decode e) (LBS.toChunks str)
	-- | Whether or no a given 'ByteString' is decodable. Default: 'True'.
	decodable :: enc -> ByteString -> Bool
	decodable _ _ = True

encodeMultibyte :: (Char -> (Word8,EncodeState)) -> String -> ByteString
encodeMultibyte f str = unfoldr (\st -> case st of
	(Done,[]) -> Nothing
	(Done,x:xs) -> let (w,st) = f x in Just (w,(st,xs))
	(Put1 w1,xs) -> Just (w1,(Done,xs))
	(Put2 w1 w2,xs) -> Just (w1,(Put1 w2,xs))
	(Put3 w1 w2 w3,xs) -> Just (w1,(Put2 w2 w3,xs))) (Done,str)

encodeMultibyteLazy :: (Char -> (Word8,EncodeState)) -> String -> LBS.ByteString
encodeMultibyteLazy f str = LBS.unfoldr (\ ~(st,rest) -> case st of
	Done -> case rest of
		[] -> Nothing
		x:xs -> let ~(w,st) = f x in Just (w,(st,xs)) 
	Put1 w1 -> Just (w1,(Done,rest))
	Put2 w1 w2 -> Just (w1,(Put1 w2,rest))
	Put3 w1 w2 w3 -> Just (w1,(Put2 w2 w3,rest))) (Done,str)

decodeMultibyte :: ([Word8] -> (Char,[Word8])) -> ByteString -> String
decodeMultibyte f str = decode (unpack str)
	where
	decode lst = let (c,nlst) = f lst in if null lst then [] else c:(decode nlst)

decodeMultibyteLazy :: ([Word8] -> (Char,[Word8])) -> LBS.ByteString -> String
decodeMultibyteLazy f str = decode (LBS.unpack str)
	where
	decode lst = let (c,nlst) = f lst in if null lst then [] else c:(decode nlst)

encodeSinglebyte :: (Char -> Word8) -> String -> ByteString
encodeSinglebyte f str = fst $ unfoldrN (Prelude.length str) (\st -> case st of
	[] -> Nothing
	(x:xs) -> Just (f x,xs)) str

encodeSinglebyteLazy :: (Char -> Word8) -> String -> LBS.ByteString
encodeSinglebyteLazy f str = LBS.unfoldr (\st -> case st of
	[] -> Nothing
	(x:xs) -> Just (f x,xs)) str

decodeSinglebyte :: (Word8 -> Char) -> ByteString -> String
decodeSinglebyte f str = map f (unpack str)

data EncodeState
	= Done
	| Put1 !Word8
	| Put2 !Word8 !Word8
	| Put3 !Word8 !Word8 !Word8
	deriving Show

-- | This exception type is thrown whenever something went wrong during the
--   encoding-process.
data EncodingException
	= HasNoRepresentation Char	-- ^ Thrown if a specific character
					--   is not representable in an encoding.
	deriving (Eq,Show,Typeable)

-- | This exception type is thrown whenever something went wrong during the
--   decoding-process.
data DecodingException
	= IllegalCharacter Word8	-- ^ The sequence contained an illegal
					--   byte that couldn't be decoded.
	| UnexpectedEnd			-- ^ more bytes were needed to allow a
					--   successfull decoding.
	| OutOfRange			-- ^ the decoded value was out of the unicode range
	| IllegalRepresentation [Word8]	-- ^ The character sequence encodes a
					--   character, but is illegal.
	deriving (Eq,Show,Typeable)

decodingArray :: FilePath -> Q Exp
decodingArray file = do
	trans <- runIO (readTranslation file)
	createCharArray trans 0 255

encodingMap :: FilePath -> Q Exp
#ifndef __HADDOCK__
encodingMap file = do
	trans <- runIO (readTranslation file)
	return $ AppE
		(VarE 'fromList)
		(ListE [ TupE [LitE $ CharL to,LitE $ IntegerL from]
			| (from,to) <- trans])
#endif

readTranslation :: FilePath -> IO [(Integer,Char)]
readTranslation file = do
	cont <- readFile file
	return $ mapMaybe (\ln -> case ln of
		[] -> Nothing
		('#':xs) -> Nothing
		_ -> case words ln of
			(src:"#UNDEFINED":_) -> Just (read src,'\xFFFD') -- XXX: Find a better way to handle this
			(src:trg:_) -> Just (read src,chr $ read trg)
			_ -> Nothing
			) (lines cont)

