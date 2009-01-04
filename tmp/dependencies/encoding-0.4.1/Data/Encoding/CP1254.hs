{-# LANGUAGE CPP,TemplateHaskell,DeriveDataTypeable #-}
{- | This module implements Windows Codepage number 1254 which encodes the turkish language.
     See <http://en.wikipedia.org/wiki/CP1254> for more information.
 -}
module Data.Encoding.CP1254
	(CP1254(..)) where

import Data.Array ((!),Array)
import Data.Word (Word8)
import Data.ByteString (all)
import Data.Map (Map,lookup,member)
import Data.Encoding.Base
import Prelude hiding (lookup,all)
import Control.OldException (throwDyn)
import Data.Typeable

data CP1254 = CP1254 deriving (Eq,Show,Typeable)

instance Encoding CP1254 where
	encode _ = encodeSinglebyte (\c -> case lookup c encodeMap of
		Just v -> v
		Nothing -> throwDyn (HasNoRepresentation c))
	encodable _ c = member c encodeMap
	decode _ = decodeSinglebyte (decodeArr!)
	decodable _ = all (\w -> decodeArr!w /= '\xFFFD')

decodeArr :: Array Word8 Char
#ifndef __HADDOCK__
decodeArr = $(decodingArray "CP1254.TXT")
#endif

encodeMap :: Map Char Word8
#ifndef __HADDOCK__
encodeMap =  $(encodingMap "CP1254.TXT")
#endif
