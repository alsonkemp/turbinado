{-# LANGUAGE CPP,TemplateHaskell #-}
{- This module is used to create arrays from lists in template haskell -}

module Data.Encoding.Helper.Template where

import Data.Char
import Data.Word
import Data.Array.IArray (Array,array)
import Language.Haskell.TH

createCharArray :: [(Integer,Char)] -> Integer -> Integer -> Q Exp
#ifndef __HADDOCK__
createCharArray lst = createArray (map (\(x,y) -> (x,LitE $ CharL y)) lst)
#endif

createArray :: [(Integer,Exp)] -> Integer -> Integer -> Q Exp
#ifndef __HADDOCK__
createArray lst from to = return $ AppE
	(AppE
		(VarE 'array)
		(TupE [LitE $ IntegerL from,LitE $ IntegerL to]))
	(ListE [ TupE [LitE $ IntegerL x,y]
		| (x,y) <- lst ])
#endif

xmlArray :: [(Char,[Word8])] -> Integer -> Integer -> Q Exp
#ifndef __HADDOCK__
xmlArray lst l u = do
	let trans = map (\(ch,bin) ->
		(toInteger $ ord ch
		,TupE [LitE $ IntegerL (toInteger $ length bin),TupE $ map (\b -> LitE $ IntegerL (fromIntegral b)) bin ++ replicate (4-length bin) (LitE $ IntegerL 0)]
		)) (filter (\(c,_) -> ord c <= fromInteger u && ord c >= fromInteger l) lst)
	createArray trans l u
#endif
