import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import qualified Data.String.UTF8     as UTF8
import qualified Codec.Binary.UTF8.String as List

import System.Environment(getArgs)
import System.IO
import Data.Word

main  = mapM_ run_test =<< getArgs

run_test x  = case reads x of
                [(n,"")] | n < test_num -> tests !! n
                _ -> hPutStrLn stderr ("Invalid test: " ++ x)

tests     = [ main0, main1, main2, main3, main4 ]
test_num  = length tests


main0 = do putStrLn "Correctness: Data.ByteString"
           putStrLn ("Errors: " ++ show encodeDecodeTest)

main1 = do putStrLn "Speed: Data.ByteString"
           txt <- S.readFile "test"
           print (UTF8.length $ UTF8.fromRep txt)

main2 = do putStrLn "Speed: Data.ByteString.Lazy"
           txt <- L.readFile "test"
           print (UTF8.length $ UTF8.fromRep txt)

main3 = do putStrLn "Speed: [Word8]"
           txt <- hGetContents =<< openBinaryFile "test" ReadMode
           let bytes :: [Word8]
               bytes = map (fromIntegral . fromEnum) txt
           print (UTF8.length $ UTF8.fromRep bytes)

main4 = do putStrLn "Speed: [Word8] (direct)"
           txt <- hGetContents =<< openBinaryFile "test" ReadMode
           let bytes :: [Word8]
               bytes = map (fromIntegral . fromEnum) txt
           print (length $ List.decode bytes)

encodeDecodeTest :: String
encodeDecodeTest =
     filter (\x -> enc x /= [x]) legal_codepoints
  ++ filter (\x -> enc x /= [UTF8.replacement_char]) illegal_codepoints
  where
    legal_codepoints    = ['\0'..'\xd7ff'] ++ ['\xe000'..'\xfffd']
                       ++ ['\x10000'..'\x10ffff']
    illegal_codepoints  = '\xffff' : '\xfffe' : ['\xd800'..'\xdfff']

{-# INLINE enc #-}
enc x = UTF8.toString (UTF8.fromString [x] :: UTF8.UTF8 S.ByteString)


