module Data.Encoding.Helper.XML where

import Data.Char
import Data.List (find)
import Data.Word
import Data.Maybe (mapMaybe)
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types
import Text.XML.HaXml.Verbatim
import Numeric

readDecodeTable :: String -> String -> [(Char,[Word8])]
readDecodeTable name str = let
	Document _ _ (Elem root_name _ conts) _ = xmlParse name str
	in concat $ mapMaybe findAssignments conts

findAssignments :: Content i -> Maybe [(Char,[Word8])]
findAssignments (CElem (Elem "assignments" _ conts) _)
	= Just $ mapMaybe findAssignment conts
findAssignments _ = Nothing

findAssignment :: Content i -> Maybe (Char,[Word8])
findAssignment (CElem (Elem "a" attrs _) _) = do
	u <- lookup "u" attrs
	b <- lookup "b" attrs
	return (chr $ readHexInt (showAttValue u),parseBinary b)
findAssignment _ = Nothing

parseBinary :: AttValue -> [Word8]
parseBinary val = map (fromIntegral.readHexInt) (words (showAttValue val))

showAttValue :: AttValue -> String
showAttValue (AttValue lst) = concat $ map (\el -> case el of
	Left str -> str
	Right ref -> verbatim ref) lst

readHexInt :: String -> Int
readHexInt str = case find (\x -> snd x == "") (readHex str) of
	Nothing -> error "Not a hex"
	Just (x,_) -> x
	
