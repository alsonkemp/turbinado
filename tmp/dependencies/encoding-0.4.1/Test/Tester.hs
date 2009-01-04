{-# LANGUAGE ExistentialQuantification #-}
module Test.Tester where

import Data.Encoding
import Test.HUnit
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Exception (catchDyn,evaluate)

data EncodingTest
	= forall enc. (Encoding enc,Show enc) =>
		EncodingTest enc String [Word8]
	| forall enc. (Encoding enc,Show enc) =>
		DecodingError enc [Word8] DecodingException
	| forall enc. (Encoding enc,Show enc) =>
		EncodingError enc String EncodingException

instance Testable EncodingTest where
	test (EncodingTest enc src trg) = TestList
		[TestLabel (show enc ++ " encodable")
			(TestCase $ (all (encodable enc) src) @=? True)
		,TestLabel (show enc ++ " encoding (strict)")
			(TestCase $ bstr @=? (encode enc src))
		,TestLabel (show enc ++ " encoding (lazy)")
			(TestCase $ lbstr @=? (encodeLazy enc src))
		,TestLabel (show enc ++ " decodable")
			(TestCase $ (decodable enc bstr) @=? True)
		,TestLabel (show enc ++ " decoding (strict)")
			(TestCase $ src @=? (decode enc bstr))
		,TestLabel (show enc ++ " decoding (lazy)")
			(TestCase $ src @=? (decodeLazy enc lbstr))
		]
		where
		bstr = BS.pack trg
		lbstr = LBS.pack trg
	test (DecodingError enc trg what) = TestList
		[TestLabel (show what++" not decodable in "++show enc) $
			TestCase $ assert $ not $ decodable enc (BS.pack trg)
		,TestLabel (show enc ++ " decoding error (strict)") $ TestCase $ 
			catchDyn (do
				mapM_ evaluate (decode enc (BS.pack trg))
				assertFailure "No exception thrown"
				)
				(\exc -> exc @=? what)
		,TestLabel (show enc ++ " decoding error (lazy)") $ TestCase $ 
			catchDyn (do
				mapM_ evaluate (decodeLazy enc (LBS.pack trg))
				assertFailure "No exception thrown"
				)
				(\exc -> exc @=? what)
		]
	test (EncodingError enc src what) = TestList
		[TestLabel (show src ++ " not encodable in " ++ show enc) $
			TestCase $ assert $ not $ all (encodable enc) src
		,TestLabel (show enc ++ " encoding error (strict)") $ TestCase $
			catchDyn (do
				evaluate (encode enc src)
				assertFailure "No exception thrown"
				)
				(\exc -> exc @=? what)
		,TestLabel (show enc ++ " encoding error (lazy)") $ TestCase $
			catchDyn (do
				evaluate (encodeLazy enc src)
				assertFailure "No exception thrown"
				)
				(\exc -> exc @=? what)
		]
