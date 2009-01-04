{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -w #-}

module System.Plugins.ParsePkgConfCabal ( 
        parsePkgConf, parseOnePkgConf
  ) where

import Distribution.InstalledPackageInfo
import Distribution.Package hiding (depends)
import Distribution.Version

import Data.Char             ( isSpace, isAlpha, isAlphaNum, isUpper, isDigit )
import Data.List             ( break )
import Data.Array
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif

-- parser produced by Happy Version 1.15

newtype HappyAbsSyn  = HappyAbsSyn (() -> ())
happyIn5 :: ([ PackageConfig ]) -> (HappyAbsSyn )
happyIn5 x = unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> ([ PackageConfig ])
happyOut5 x = unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: ([ PackageConfig ]) -> (HappyAbsSyn )
happyIn6 x = unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> ([ PackageConfig ])
happyOut6 x = unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (PackageConfig) -> (HappyAbsSyn )
happyIn7 x = unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (PackageConfig)
happyOut7 x = unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (PackageConfig -> PackageConfig) -> (HappyAbsSyn )
happyIn8 x = unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (PackageConfig -> PackageConfig)
happyOut8 x = unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (PackageConfig -> PackageConfig) -> (HappyAbsSyn )
happyIn9 x = unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (PackageConfig -> PackageConfig)
happyOut9 x = unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (PackageIdentifier) -> (HappyAbsSyn )
happyIn10 x = unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (PackageIdentifier)
happyOut10 x = unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (Version) -> (HappyAbsSyn )
happyIn11 x = unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (Version)
happyOut11 x = unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: ([PackageIdentifier]) -> (HappyAbsSyn )
happyIn12 x = unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> ([PackageIdentifier])
happyOut12 x = unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: ([PackageIdentifier]) -> (HappyAbsSyn )
happyIn13 x = unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> ([PackageIdentifier])
happyOut13 x = unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ([Int]) -> (HappyAbsSyn )
happyIn14 x = unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ([Int])
happyOut14 x = unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: ([Int]) -> (HappyAbsSyn )
happyIn15 x = unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> ([Int])
happyOut15 x = unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: ([String]) -> (HappyAbsSyn )
happyIn16 x = unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> ([String])
happyOut16 x = unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: ([String]) -> (HappyAbsSyn )
happyIn17 x = unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> ([String])
happyOut17 x = unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyInTok :: Token -> (HappyAbsSyn )
happyInTok x = unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> Token
happyOutTok x = unsafeCoerce# x
{-# INLINE happyOutTok #-}

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x50\x00\x4a\x00\x4c\x00\x49\x00\x46\x00\x4b\x00\x45\x00\x0a\x00\x1e\x00\x00\x00\x00\x00\x44\x00\x16\x00\x00\x00\x43\x00\x00\x00\x42\x00\x00\x00\x03\x00\x00\x00\x41\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x01\x00\x00\x00\x40\x00\x00\x00\x3e\x00\x3d\x00\x1c\x00\x00\x00\x3f\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x3a\x00\x39\x00\x35\x00\x00\x00\x00\x00\x38\x00\x31\x00\x34\x00\x33\x00\x37\x00\x36\x00\x28\x00\x00\x00\x30\x00\x32\x00\x2f\x00\x09\x00\x2d\x00\x00\x00\x2e\x00\x26\x00\x2c\x00\x22\x00\x00\x00\x00\x00\x2b\x00\x29\x00\x0d\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x2a\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x00\x00\x00\xfe\xff\x00\x00\x21\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x00\x00\x1a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x04\x00\x00\x00\xfb\xff\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfb\xff\xfd\xff\x00\x00\x00\x00\xf8\xff\x00\x00\xfc\xff\x00\x00\xfa\xff\x00\x00\xf9\xff\x00\x00\xf7\xff\xf6\xff\xf1\xff\xf2\xff\x00\x00\xf4\xff\xf5\xff\x00\x00\xf3\xff\xed\xff\x00\x00\x00\x00\xe7\xff\x00\x00\xe5\xff\xe6\xff\x00\x00\xee\xff\x00\x00\x00\x00\x00\x00\xec\xff\xe4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xeb\xff\xe9\xff\x00\x00\x00\x00\x00\x00\xea\xff\xe8\xff\x00\x00\x00\x00\x00\x00\xef\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x05\x00\x01\x00\x05\x00\x08\x00\x07\x00\x03\x00\x0c\x00\x0c\x00\x0b\x00\x09\x00\x08\x00\x09\x00\x04\x00\x04\x00\x0b\x00\x04\x00\x04\x00\x08\x00\x0a\x00\x08\x00\x09\x00\x09\x00\x05\x00\x02\x00\x0a\x00\x08\x00\x05\x00\x03\x00\x04\x00\x01\x00\x02\x00\x04\x00\x05\x00\x04\x00\x05\x00\x0a\x00\x04\x00\x06\x00\x02\x00\x09\x00\x02\x00\x00\x00\x02\x00\x0a\x00\x07\x00\x03\x00\x07\x00\xff\xff\x04\x00\x06\x00\x05\x00\x05\x00\x03\x00\x06\x00\x01\x00\x07\x00\x02\x00\x06\x00\x08\x00\xff\xff\x05\x00\x09\x00\x06\x00\x01\x00\x04\x00\x08\x00\x05\x00\x09\x00\xff\xff\xff\xff\x07\x00\x07\x00\x06\x00\x08\x00\x07\x00\x01\x00\x04\x00\xff\xff\x03\x00\x0b\x00\x0b\x00\x08\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x1e\x00\x1d\x00\x16\x00\x1f\x00\x17\x00\x1a\x00\x20\x00\x20\x00\x18\x00\x1e\x00\x1b\x00\x1c\x00\x3a\x00\x0b\x00\x41\x00\x22\x00\x22\x00\x06\x00\x3b\x00\x23\x00\x24\x00\x24\x00\x1e\x00\x14\x00\x3f\x00\x2a\x00\x15\x00\x0c\x00\x0d\x00\x08\x00\x09\x00\x25\x00\x26\x00\x10\x00\x11\x00\x38\x00\x15\x00\x30\x00\x11\x00\x36\x00\x04\x00\x06\x00\x44\x00\x3b\x00\x3d\x00\x43\x00\x35\x00\x00\x00\x3f\x00\x41\x00\x3e\x00\x3c\x00\x38\x00\x36\x00\x33\x00\x2f\x00\x34\x00\x30\x00\x32\x00\x00\x00\x2e\x00\x2d\x00\x2a\x00\x1d\x00\x27\x00\x23\x00\x28\x00\x2c\x00\x00\x00\x00\x00\x29\x00\x0f\x00\x13\x00\x06\x00\x0f\x00\x0c\x00\x0b\x00\x00\x00\x04\x00\xff\xff\xff\xff\x06\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = array (2, 27) [
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27)
	]

happy_n_terms = 12 :: Int
happy_n_nonterms = 13 :: Int

happyReduce_2 = happySpecReduce_2 0# happyReduction_2
happyReduction_2 happy_x_2
	happy_x_1
	 =  happyIn5
		 ([]
	)

happyReduce_3 = happySpecReduce_3 0# happyReduction_3
happyReduction_3 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_2 of { happy_var_2 -> 
	happyIn5
		 (reverse happy_var_2
	)}

happyReduce_4 = happySpecReduce_1 1# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn6
		 ([ happy_var_1 ]
	)}

happyReduce_5 = happySpecReduce_3 1# happyReduction_5
happyReduction_5 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn6
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_6 = happyReduce 4# 2# happyReduction_6
happyReduction_6 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 (happy_var_3 defaultPackageConfig
	) `HappyStk` happyRest}

happyReduce_7 = happySpecReduce_1 3# happyReduction_7
happyReduction_7 happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (\p -> happy_var_1 p
	)}

happyReduce_8 = happySpecReduce_3 3# happyReduction_8
happyReduction_8 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	happyIn8
		 (\p -> happy_var_1 (happy_var_3 p)
	)}}

happyReduce_9 = happySpecReduce_3 4# happyReduction_9
happyReduction_9 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (ITvarid    happy_var_1) -> 
	case happyOut10 happy_x_3 of { happy_var_3 -> 
	happyIn9
		 (\p -> case happy_var_1 of
		   "package" -> p {package = happy_var_3}
		   _      -> error "unknown key in config file"
	)}}

happyReduce_10 = happySpecReduce_3 4# happyReduction_10
happyReduction_10 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn9
		 (id
	)

happyReduce_11 = happySpecReduce_3 4# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (ITvarid    happy_var_1) -> 
	case happyOutTok happy_x_3 of { (ITconid    happy_var_3) -> 
	happyIn9
		 (case happy_var_1 of {
		   	"exposed" -> 
			   case happy_var_3 of {
				"True"  -> (\p -> p {exposed=True});
				"False" -> (\p -> p {exposed=False});
				_       -> error "exposed must be either True or False" };
		   	"license" -> id; -- not interested
		   	_         -> error "unknown constructor" }
	)}}

happyReduce_12 = happyReduce 4# 4# happyReduction_12
happyReduction_12 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyIn9
		 (id
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_3 4# happyReduction_13
happyReduction_13 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (ITvarid    happy_var_1) -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn9
		 (\p -> case happy_var_1 of
		        "exposedModules"    -> p{exposedModules    = happy_var_3}
		        "hiddenModules"     -> p{hiddenModules     = happy_var_3}
		        "importDirs"        -> p{importDirs        = happy_var_3}
		        "libraryDirs"       -> p{libraryDirs       = happy_var_3}
		        "hsLibraries"       -> p{hsLibraries       = happy_var_3}
		        "extraLibraries"    -> p{extraLibraries    = happy_var_3}
		        "includeDirs"       -> p{includeDirs       = happy_var_3}
		        "includes"          -> p{includes          = happy_var_3}
		        "hugsOptions"       -> p{hugsOptions       = happy_var_3}
		        "ccOptions"         -> p{ccOptions         = happy_var_3}
		        "ldOptions"         -> p{ldOptions         = happy_var_3}
		        "frameworkDirs"     -> p{frameworkDirs     = happy_var_3}
		        "frameworks"        -> p{frameworks        = happy_var_3}
		        "haddockInterfaces" -> p{haddockInterfaces = happy_var_3}
		        "haddockHTMLs"      -> p{haddockHTMLs      = happy_var_3}
		        "depends"     	    -> p{depends = []}
				-- empty list only, non-empty handled below
			other -> p
	)}}

happyReduce_14 = happySpecReduce_3 4# happyReduction_14
happyReduction_14 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (ITvarid    happy_var_1) -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn9
		 (case happy_var_1 of
		        "depends"     -> (\p -> p{depends = happy_var_3})
			_other        -> error "unknown key in config file"
	)}}

happyReduce_15 = happyReduce 10# 5# happyReduction_15
happyReduction_15 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_5 of { (ITstring   happy_var_5) -> 
	case happyOut11 happy_x_9 of { happy_var_9 -> 
	happyIn10
		 (PackageIdentifier{ pkgName = happy_var_5, 
					     pkgVersion = happy_var_9 }
	) `HappyStk` happyRest}}

happyReduce_16 = happyReduce 10# 6# happyReduction_16
happyReduction_16 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut14 happy_x_5 of { happy_var_5 -> 
	case happyOut16 happy_x_9 of { happy_var_9 -> 
	happyIn11
		 (Version{ versionBranch=happy_var_5, versionTags=happy_var_9 }
	) `HappyStk` happyRest}}

happyReduce_17 = happySpecReduce_3 7# happyReduction_17
happyReduction_17 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_2 of { happy_var_2 -> 
	happyIn12
		 (happy_var_2
	)}

happyReduce_18 = happySpecReduce_1 8# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 ([ happy_var_1 ]
	)}

happyReduce_19 = happySpecReduce_3 8# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_20 = happySpecReduce_2 9# happyReduction_20
happyReduction_20 happy_x_2
	happy_x_1
	 =  happyIn14
		 ([]
	)

happyReduce_21 = happySpecReduce_3 9# happyReduction_21
happyReduction_21 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn14
		 (happy_var_2
	)}

happyReduce_22 = happySpecReduce_1 10# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOutTok happy_x_1 of { (ITinteger  happy_var_1) -> 
	happyIn15
		 ([ fromIntegral happy_var_1 ]
	)}

happyReduce_23 = happySpecReduce_3 10# happyReduction_23
happyReduction_23 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (ITinteger  happy_var_1) -> 
	case happyOut15 happy_x_3 of { happy_var_3 -> 
	happyIn15
		 (fromIntegral happy_var_1 : happy_var_3
	)}}

happyReduce_24 = happySpecReduce_2 11# happyReduction_24
happyReduction_24 happy_x_2
	happy_x_1
	 =  happyIn16
		 ([]
	)

happyReduce_25 = happySpecReduce_3 11# happyReduction_25
happyReduction_25 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_2 of { happy_var_2 -> 
	happyIn16
		 (reverse happy_var_2
	)}

happyReduce_26 = happySpecReduce_1 12# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOutTok happy_x_1 of { (ITstring   happy_var_1) -> 
	happyIn17
		 ([ happy_var_1 ]
	)}

happyReduce_27 = happySpecReduce_3 12# happyReduction_27
happyReduction_27 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_3 of { (ITstring   happy_var_3) -> 
	happyIn17
		 (happy_var_3 : happy_var_1
	)}}

happyNewToken action sts stk [] =
	happyDoAction 11# (error "reading EOF!") action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	ITocurly -> cont 1#;
	ITccurly -> cont 2#;
	ITobrack -> cont 3#;
	ITcbrack -> cont 4#;
	ITcomma -> cont 5#;
	ITequal -> cont 6#;
	ITvarid    happy_dollar_dollar -> cont 7#;
	ITconid    happy_dollar_dollar -> cont 8#;
	ITstring   happy_dollar_dollar -> cont 9#;
	ITinteger  happy_dollar_dollar -> cont 10#;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [Token] -> HappyIdentity a
happyError' = HappyIdentity . happyError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut5 x))

parseOne tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut7 x))

happySeq = happyDoSeq

type PackageConfig = InstalledPackageInfo

defaultPackageConfig = emptyInstalledPackageInfo

data Token 
        = ITocurly
        | ITccurly
        | ITobrack
        | ITcbrack
        | ITcomma
        | ITequal
        | ITvarid String
        | ITconid String
        | ITstring String
        | ITinteger Int

lexer :: String -> [Token]

lexer [] = []
lexer ('{':cs) = ITocurly : lexer cs
lexer ('}':cs) = ITccurly : lexer cs
lexer ('[':cs) = ITobrack : lexer cs
lexer (']':cs) = ITcbrack : lexer cs
lexer (',':cs) = ITcomma : lexer cs
lexer ('=':cs) = ITequal : lexer cs
lexer ('"':cs) = lexString cs ""
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = lexID (c:cs) 
    | isDigit c = lexInt (c:cs)
lexer _ = error ( "Unexpected token")

lexID cs = (if isUpper (head cs) then ITconid else ITvarid) id : lexer rest
    where
	(id,rest) = break (\c -> c /= '_' && not (isAlphaNum c)) cs

lexInt cs = let (intStr, rest) = span isDigit cs
            in  ITinteger (read intStr) : lexer rest


lexString ('"':cs) s = ITstring (reverse s) : lexer cs
lexString ('\\':c:cs) s = lexString cs (c:s)
lexString (c:cs) s = lexString cs (c:s)

happyError _ = error "Couldn't parse package configuration."

parsePkgConf :: String -> [PackageConfig]
parsePkgConf = parse . lexer

parseOnePkgConf :: String -> PackageConfig
parseOnePkgConf = parseOne . lexer
{-# LINE 1 "GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "GenericTemplate.hs" #-}
-- $Id$


{-# LINE 28 "GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Int# Happy_IntList






{-# LINE 49 "GenericTemplate.hs" #-}


{-# LINE 59 "GenericTemplate.hs" #-}










infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n <# (0# :: Int#)) -> {- nothing -}

				     (happyReduceArr ! rule) i tk st
				     where rule = (I# ((negateInt# ((n +# (1# :: Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where new_state = (n -# (1# :: Int#))
   where off    = indexShortOffAddr happyActOffsets st
	 off_i  = (off +# i)
	 check  = if (off_i >=# (0# :: Int#))
			then (indexShortOffAddr happyCheck off_i ==#  i)
			else False
 	 action | check     = indexShortOffAddr happyTable off_i
		| otherwise = indexShortOffAddr happyDefActions st











indexShortOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ > 500
	narrow16Int# i
#elif __GLASGOW_HASKELL__ == 500
	intToInt16# i
#else
	(i `iShiftL#` 16#) `iShiftRA#` 16#
#endif
  where
#if __GLASGOW_HASKELL__ >= 503
	i = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
#else
	i = word2Int# ((high `shiftL#` 8#) `or#` low)
#endif
	high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
	low  = int2Word# (ord# (indexCharOffAddr# arr off'))
	off' = off *# 2#





data HappyAddr = HappyA# Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case unsafeCoerce# x of { (I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k -# (1# :: Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n -# (1# :: Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n -# (1#::Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = (off +# nt)
 	 new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  0# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (unsafeCoerce# (I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
