module System.IO.Encoding
	(getSystemEncoding
	,hPutStr
	,hGetContents) where

import Foreign.C.String

import Data.Encoding
import System.IO hiding (hPutStr,hGetContents)
import qualified Data.ByteString.Lazy as BS

-- | Like the normal 'System.IO.hGetContents', but decodes the input using an
--   encoding.
hGetContents :: Encoding e => e -> Handle -> IO String
hGetContents enc h = do
	str <- BS.hGetContents h
	return $ decodeLazy enc str

-- | Like the normal 'System.IO.hPutStr', but encodes the output using an
--   encoding.
hPutStr :: Encoding e => e -> Handle -> String -> IO ()
hPutStr enc h str = BS.hPut h (encodeLazy enc str)

foreign import ccall "system_encoding.h get_system_encoding"
	get_system_encoding :: IO CString

-- | Returns the encoding used on the current system.
getSystemEncoding :: IO DynEncoding
getSystemEncoding = do
	enc <- get_system_encoding
	str <- peekCString enc
	return $ encodingFromString str
