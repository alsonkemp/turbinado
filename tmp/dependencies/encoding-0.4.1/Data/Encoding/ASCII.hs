{-# LANGUAGE DeriveDataTypeable #-}
-- | ASCII (American Standard Code for Information Interchange) is the
--   \"normal\" computer encoding using the byte values 0-127 to represent
--   characters. Refer to <http://en.wikipedia.org/wiki/ASCII> for
--   more information.
module Data.Encoding.ASCII
	(ASCII(..)) where

import Control.OldException (throwDyn)
import Data.ByteString (pack)
import qualified Data.ByteString.Lazy as Lazy (pack)
import Data.ByteString.Char8 (unpack)
import Data.Char (ord)
import qualified Data.ByteString.Lazy as Lazy
import Data.Encoding.Base
import Data.Word
import Data.Typeable

data ASCII = ASCII deriving (Show,Eq,Typeable)

charToASCII :: Char -> Word8
charToASCII ch = if ch < '\128'
	then fromIntegral $ ord ch
	else throwDyn (HasNoRepresentation ch)

instance Encoding ASCII where
	encode _ str = pack (map charToASCII str)
	encodeLazy _ str = Lazy.pack (map charToASCII str)
	encodable _ ch = ch < '\128'
	decode _ = unpack
	decodable _ = const True
