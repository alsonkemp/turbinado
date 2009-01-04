{-# LANGUAGE CPP,TemplateHaskell,DeriveDataTypeable #-}
{- | This module implements Windows Codepage number 1258 which encodes the vietnamese language.
     See <http://en.wikipedia.org/wiki/CP1258> for more information.
 -}
module Data.Encoding.CP1258
	(CP1258(..)) where

import Data.Array ((!),Array)
import Data.Word (Word8)
import Data.ByteString (all)
import Data.Map (Map,lookup,member)
import Data.Encoding.Base
import Prelude hiding (lookup,all)
import Control.OldException (throwDyn)
import Data.Typeable

data CP1258 = CP1258 deriving (Eq,Show,Typeable)

instance Encoding CP1258 where
	encode _ = encodeSinglebyte (\c -> case lookup c encodeMap of
		Just v -> v
		Nothing -> throwDyn (HasNoRepresentation c))
	encodable _ c = member c encodeMap
	decode _ = decodeSinglebyte (decodeArr!)
	decodable _ = all (\w -> decodeArr!w /= '\xFFFD')

decodeArr :: Array Word8 Char
#ifndef __HADDOCK__
decodeArr = $(decodingArray "CP1258.TXT")
#endif

encodeMap :: Map Char Word8
#ifndef __HADDOCK__
encodeMap =  $(encodingMap "CP1258.TXT")
#endif
