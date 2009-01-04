import Codec.Binary.UTF8.String
import Test.HUnit
import System.Exit (exitFailure)
import Control.Monad (when)

main = do counts <- runTestTT tests
          when (errors counts > 0 || failures counts > 0) exitFailure

tests = TestList [test_1, test_2, test_3, test_4, test_5]

test_1 = TestLabel "1 Some correct UTF-8 text" $
  TestCase $ assertEqual "kosme, " "\x03ba\x1f79\x03c3\x03bc\x03b5 "
    (decode [0xce,0xba,0xe1,0xbd,0xb9,0xcf,0x83,0xce,0xbc,0xce,0xb5,0x20])

test_2 = TestLabel "2 Boundary condition test cases" $
  TestList [test_2_1, test_2_2, test_2_3]

test_2_1 = TestLabel "2.1 First possible sequence of a certain length" $
  TestList $ map TestCase $ 
  [ assertEqual "2.1.1, " "\0\0" (decode [0, 0])
  , assertEqual "2.1.2, " "\x80\0" (decode [0xc2, 0x80, 0])
  , assertEqual "2.1.3, " "\x800\0" (decode [0xe0, 0xa0, 0x80, 0])
  , assertEqual "2.1.4, " "\x10000\0" (decode [0xf0, 0x90, 0x80, 0x80, 0])
  , assertEqual "2.1.5, " "\xfffd\0" (decode [0xf8, 0x88, 0x80, 0x80, 0x80, 0])
  , assertEqual "2.1.6, " "\xfffd\0" (decode [0xfc,0x84,0x80,0x80,0x80,0x80,0])
  ]

test_2_2 = TestLabel "2.2 Last possible sequence of a certain length" $
  TestList $ map TestCase $ 
  [ assertEqual "2.2.1, " "\x7f\0" (decode [0x7f, 0])
  , assertEqual "2.2.2, " "\x7ff\0" (decode [0xdf, 0xbf, 0])
  , assertEqual "2.2.3, " "\xfffd\0" (decode [0xef, 0xbf, 0xbf, 0])
  , assertEqual "2.2.4, " "\xfffd\0" (decode [0xf7, 0xbf, 0xbf, 0xbf, 0])
  , assertEqual "2.2.5, " "\xfffd\0" (decode [0xfb, 0xbf, 0xbf, 0xbf, 0xbf, 0])
  , assertEqual "2.2.6, " "\xfffd\0" (decode [0xfd,0xbf,0xbf,0xbf,0xbf,0xbf,0])
  ]

test_2_3 = TestLabel "2.3 Other boundary conditions" $
  TestList $ map TestCase $ 
  [ assertEqual "2.3.1, " "\xd7ff\0" (decode [0xed, 0x9f, 0xbf, 0])
  , assertEqual "2.3.2, " "\xe000\0" (decode [0xee, 0x80, 0x80, 0])
  , assertEqual "2.3.3, " "\xfffd\0" (decode [0xef, 0xbf, 0xbd, 0])
  , assertEqual "2.3.4, " "\x10ffff\0" (decode [0xf4, 0x8f, 0xbf, 0xbf, 0])
  , assertEqual "2.3.5, " "\xfffd\0" (decode [0xf4, 0x90, 0x80, 0x80, 0])
  ]

test_3 = TestLabel "3 Malformed sequences" $
  TestList [test_3_1, test_3_2, test_3_3, test_3_4, test_3_5]

test_3_1 = TestLabel "3.1 Unexpected continuation bytes" $
  TestList $ map TestCase $
  [ assertEqual "3.1.1, " "\xfffd\0" (decode [0x80, 0])
  , assertEqual "3.1.2, " "\xfffd\0" (decode [0xbf, 0])
  , assertEqual "3.1.3, " "\xfffd\xfffd\0" (decode [0x80, 0xbf, 0])
  , assertEqual "3.1.4, " "\xfffd\xfffd\xfffd\0" (decode [0x80, 0xbf, 0x80, 0])
  , assertEqual "3.1.5, " "\xfffd\xfffd\xfffd\xfffd\0"
                          (decode [0x80, 0xbf, 0x80, 0xbf, 0])
  , assertEqual "3.1.6, " "\xfffd\xfffd\xfffd\xfffd\xfffd\0"
                          (decode [0x80, 0xbf, 0x80, 0xbf, 0x80, 0])
  , assertEqual "3.1.7, " "\xfffd\xfffd\xfffd\xfffd\xfffd\xfffd\0"
                          (decode [0x80, 0xbf, 0x80, 0xbf, 0x80, 0xbf, 0])
  , assertEqual "3.1.8, " "\xfffd\xfffd\xfffd\xfffd\xfffd\xfffd\xfffd\0"
                          (decode [0x80, 0xbf, 0x80, 0xbf, 0x80, 0xbf, 0x80, 0])
  , assertEqual "3.1.9, " (replicate 64 '\xfffd') (decode [0x80..0xbf])
  ]

test_3_2 = TestLabel "3.2 Lonely start characters" $
  TestList $ map TestCase $
  [ assertEqual "3.2.1, " (concat (replicate 32 "\xfffd "))
                          (decode (concat [[x,0x20] | x <- [0xc0..0xdf]]))
  , assertEqual "3.2.2, " (concat (replicate 16 "\xfffd "))
                          (decode (concat [[x,0x20] | x <- [0xe0..0xef]]))
  , assertEqual "3.2.3, " (concat (replicate 8 "\xfffd "))
                          (decode (concat [[x,0x20] | x <- [0xf0..0xf7]]))
  , assertEqual "3.2.4, " "\xfffd \xfffd \xfffd \xfffd "
                          (decode (concat [[x,0x20] | x <- [0xf8..0xfb]]))
  , assertEqual "3.2.5, " "\xfffd \xfffd " (decode [0xfc, 0x20, 0xfd, 0x20])
  ]

test_3_3 = TestLabel "3.3 Sequences with last continuation byte missing" $
  TestList $ map TestCase $
  [ assertEqual "3.3.1, " "\xfffd " (decode [0xc0, 0x20])
  , assertEqual "3.3.2, " "\xfffd " (decode [0xe0, 0x80, 0x20])
  , assertEqual "3.3.3, " "\xfffd " (decode [0xf0, 0x80, 0x80, 0x20])
  , assertEqual "3.3.4, " "\xfffd " (decode [0xf8, 0x80, 0x80, 0x80, 0x20])
  , assertEqual "3.3.5, " "\xfffd " (decode [0xfc, 0x80, 0x80, 0x80,0x80,0x20])
  , assertEqual "3.3.6, " "\xfffd " (decode [0xdf, 0x20])
  , assertEqual "3.3.7, " "\xfffd " (decode [0xef, 0xbf, 0x20])
  , assertEqual "3.3.8, " "\xfffd " (decode [0xf7, 0xbf, 0xbf, 0x20])
  , assertEqual "3.3.9, " "\xfffd " (decode [0xfb, 0xbf, 0xbf, 0xbf, 0x20])
  , assertEqual "3.3.10, " "\xfffd " (decode [0xfd, 0xbf, 0xbf, 0xbf,0xbf,0x20])
  ]

test_3_4 = TestLabel "3.4 Concatenation of incomplete sequences" $
  TestCase $ assertEqual "3.4, " 
  (replicate 10 '\xfffd')
  (decode [0xc0, 0xe0, 0x80, 0xf0, 0x80, 0x80, 0xf8, 0x80, 0x80, 0x80,
   0xfc, 0x80, 0x80, 0x80,0x80, 0xdf, 0xef, 0xbf, 0xf7, 0xbf, 0xbf,
   0xfb, 0xbf, 0xbf, 0xbf, 0xfd, 0xbf, 0xbf, 0xbf,0xbf])

test_3_5 = TestLabel "3.5 Impossible bytes" $
  TestList $ map TestCase $
  [ assertEqual "3.5.1, " "\xfffd " (decode [0xfe, 0x20])
  , assertEqual "3.5.2, " "\xfffd " (decode [0xff, 0x20])
  , assertEqual "3.5.3, " "\xfffd\xfffd\xfffd\xfffd "
                          (decode [0xfe, 0xfe, 0xff, 0xff, 0x20])
  ]

test_4 = TestLabel "4 Overlong sequences" $
  TestList [test_4_1, test_4_2, test_4_3]

test_4_1 = TestLabel "4.1" $ TestList $ map TestCase $
  [ assertEqual "4.1.1, " "\xfffd " (decode [0xc0, 0xaf, 0x20])
  , assertEqual "4.1.2, " "\xfffd " (decode [0xe0, 0x80, 0xaf, 0x20])
  , assertEqual "4.1.3, " "\xfffd " (decode [0xf0, 0x80, 0x80, 0xaf, 0x20])
  , assertEqual "4.1.4, " "\xfffd " (decode [0xf8, 0x80, 0x80,0x80,0xaf, 0x20])
  , assertEqual "4.1.5, " "\xfffd " (decode[0xfc,0x80,0x80,0x80,0x80,0xaf,0x20])
  ]

test_4_2 = TestLabel "4.2 Maximum overlong sequences" $
  TestList $ map TestCase $
  [ assertEqual "4.2.1, " "\xfffd " (decode [0xc1, 0xbf, 0x20])
  , assertEqual "4.2.2, " "\xfffd " (decode [0xe0, 0x9f, 0xbf, 0x20])
  , assertEqual "4.2.3, " "\xfffd " (decode [0xf0, 0x8f, 0xbf, 0xbf, 0x20])
  , assertEqual "4.2.4, " "\xfffd " (decode [0xf8, 0x87, 0xbf, 0xbf,0xbf,0x20])
  , assertEqual "4.2.5, " "\xfffd "(decode[0xfc,0x83,0xbf,0xbf,0xbf,0xbf,0x20])
  ]

test_4_3 = TestLabel "4.2 Overlong NUL" $
  TestList $ map TestCase $
  [ assertEqual "4.3.1, " "\xfffd " (decode [0xc0, 0x80, 0x20])
  , assertEqual "4.3.2, " "\xfffd " (decode [0xe0, 0x80, 0x80, 0x20])
  , assertEqual "4.3.3, " "\xfffd " (decode [0xf0, 0x80, 0x80, 0x80, 0x20])
  , assertEqual "4.3.4, " "\xfffd " (decode [0xf8, 0x80, 0x80, 0x80,0x80,0x20])
  , assertEqual "4.3.5, " "\xfffd "(decode[0xfc,0x80,0x80,0x80,0x80,0x80,0x20])
  ]

test_5 = TestLabel "Illegal code positions" $
  TestList [test_5_1, test_5_2, test_5_3]

test_5_1 = TestLabel "5.1 Single UTF-16 surrogates" $
  TestList $ map TestCase $
  [ assertEqual "5.1.1, " "\xfffd " (decode [0xed,0xa0,0x80,0x20])
  , assertEqual "5.1.2, " "\xfffd " (decode [0xed,0xad,0xbf,0x20])
  , assertEqual "5.1.3, " "\xfffd " (decode [0xed,0xae,0x80,0x20])
  , assertEqual "5.1.4, " "\xfffd " (decode [0xed,0xaf,0xbf,0x20])
  , assertEqual "5.1.5, " "\xfffd " (decode [0xed,0xb0,0x80,0x20])
  , assertEqual "5.1.6, " "\xfffd " (decode [0xed,0xbe,0x80,0x20])
  , assertEqual "5.1.7, " "\xfffd " (decode [0xed,0xbf,0xbf,0x20])
  ]
 
test_5_2 = TestLabel "5.2 Paired UTF-16 surrogates" $
  TestList $ map TestCase $
  [ assertEqual "5.2.1, " res (decode [0xed,0xa0,0x80,0xed,0xb0,0x80,0x20])
  , assertEqual "5.2.2, " res (decode [0xed,0xa0,0x80,0xed,0xbf,0xbf,0x20])
  , assertEqual "5.2.3, " res (decode [0xed,0xad,0xbf,0xed,0xb0,0x80,0x20])
  , assertEqual "5.2.4, " res (decode [0xed,0xad,0xbf,0xed,0xbf,0xbf,0x20])
  , assertEqual "5.2.5, " res (decode [0xed,0xae,0x80,0xed,0xb0,0x80,0x20])
  , assertEqual "5.2.6, " res (decode [0xed,0xae,0x80,0xed,0xbf,0xbf,0x20])
  , assertEqual "5.2.7, " res (decode [0xed,0xaf,0xbf,0xed,0xb0,0x80,0x20])
  , assertEqual "5.2.8, " res (decode [0xed,0xaf,0xbf,0xed,0xbf,0xbf,0x20])
  ]
  where res = "\xfffd\xfffd "
 
test_5_3 = TestLabel "5.3 Other illegal code positions" $
  TestList $ map TestCase $
  [ assertEqual "5.3.1, " "\xfffd " (decode [0xef, 0xbf, 0xbe, 0x20])
  , assertEqual "5.3.2, " "\xfffd " (decode [0xef, 0xbf, 0xbf, 0x20])
  ]


--
-- test decode . encode == id for the class of chars we know that to be true of
--
encodeDecodeTest :: [Char]
encodeDecodeTest = filter (\x -> [x] /= decode (encode [x])) legal_codepoints ++
                   filter (\x -> ['\xfffd'] /= decode (encode [x])) illegal_codepoints
  where
    legal_codepoints = ['\0'..'\xd7ff'] ++ ['\xe000'..'\xfffd'] ++ ['\x10000'..'\x10ffff']
    illegal_codepoints = '\xffff' : '\xfffe' : ['\xd800'..'\xdfff']


