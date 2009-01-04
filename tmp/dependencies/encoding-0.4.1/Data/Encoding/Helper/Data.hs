{- This module is used to create a haskell file with static address-arrays in it -}

module Data.Encoding.Helper.Data where

import Data.Char (ord)
import Data.Word (Word8)
import Data.Ord (comparing)
import Data.Bits (shiftR)
import Data.List (sortBy,genericLength)
import Data.Encoding.Helper.XML

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

createStandardModule :: IO ()
createStandardModule = do
	let fn = "gb-18030-2000.xml"
	str <- readFile fn
	writeFile "Data/Encoding/GB18030Data.hs" $ createModuleFromFile fn str standardRanges standardRRanges

standardRanges =
	[('\x0000','\x0451')
	,('\x2010','\x2642')
	,('\x2E81','\x361A')
	,('\x3918','\x3CE0')
	,('\x4056','\x415F')
	,('\x4337','\x44D6')
	,('\x464C','\x478D')
	,('\x4947','\x49B7')
	,('\x4C77','\x9FA5')
	,('\xD800','\xE864')
	,('\xF92C','\xFA29')
	,('\xFE30','\xFFE5')
	]

standardRRanges =
	[(    0,  819)
	,( 7922, 9218)
	,(11329,12972)
	,(13738,14697)
	,(15583,15846)
	,(16318,16728)
	,(17102,17417)
	,(17859,17960)
	,(18664,19042)
	,(33469,33549)
	,(37845,38077)
	,(39108,39393)
	,(39420,188999)]

createModuleFromFile :: String -> String -> [(Char,Char)] -> [(Int,Int)] -> String
createModuleFromFile name str = createModule (readDecodeTable name str)

createModule :: [(Char,[Word8])] -> [(Char,Char)] -> [(Int,Int)] -> String
createModule mp ranges rranges = unlines $
	["{-# LANGUAGE CPP,MagicHash #-}"
	,"module Data.Encoding.GB18030Data where"
	,""
	,"import Data.ByteString(ByteString)"
	,"#if __GLASGOW_HASKELL__>=608"
	,"import Data.ByteString.Unsafe(unsafePackAddressLen)"
	,"#else"
	,"import Data.ByteString.Base(unsafePackAddressLen)"
	,"#endif"
	,"import System.IO.Unsafe(unsafePerformIO)"]
	++ (createAddrVars "arr" (map (uncurry $ createAddr mp) ranges))
	++ (createAddrVars "rarr" (map (uncurry $ createRAddr4 mp) rranges))
	++ (createAddrVar "rrarr" (createRAddr2 mp))

createAddrVars :: String -> [[Word8]] -> [String]
createAddrVars base conts = concatMap (\(n,cont) ->
	createAddrVar (base++show n) cont) (zip [1..] conts)

createAddrVar :: String -> [Word8] -> [String]
createAddrVar name cont =
	[""
	,name++" :: ByteString"
	,name++" = unsafePerformIO $ unsafePackAddressLen "++show (length cont)++" \""++addr cont++"\"#"
	]

createAddr :: [(Char,[Word8])] -> Char -> Char -> [Word8]
createAddr mp f t = let
	lst = sortBy (comparing fst) [el | el@(ch,_) <- mp, ch>=f, ch<=t]
	in concatMap (\(ch,seq) -> let
		l = length seq
		in [fromIntegral l]++seq++(replicate (4-l) 0)) lst

createRAddr2 :: [(Char,[Word8])] -> [Word8]
createRAddr2 mp = let
	lst = sortBy (comparing snd)
		[ (ch,v) | (ch,[w1,w2]) <- mp,let v = linear2 w1 w2]
	in concatMap (\(ch,_) -> let i = ord ch
		in [fromIntegral (i `shiftR` 8)
		   ,fromIntegral i]) lst

createRAddr4 :: [(Char,[Word8])] -> Int -> Int -> [Word8]
createRAddr4 mp f t = let
	lst = sortBy (comparing snd)
		[ (ch,v) | (ch,[w1,w2,w3,w4]) <- mp,
		let v = linear w1 w2 w3 w4, v>=f, v<=t ]
	in concatMap (\(ch,_) -> let i = ord ch
		in [fromIntegral (i `shiftR` 8)
		   ,fromIntegral i]) lst

addr :: [Word8] -> String
addr = concatMap (\w -> "\\"++show w)
