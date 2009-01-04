{-# OPTIONS -cpp -fglasgow-exts #-}

{-
$ ghc --make -O2 Bench.hs -o bench 

$ ./bench 
Size of test data: 2428k
Char    Optimal byteString decode
 1 0.102  0.109  0.102  0.109  0.102  
   0.063  0.063  0.070  0.055  0.070        # "decode"   
-}

--
-- Benchmark tool.
-- Compare a function against equivalent code from other libraries for
-- space and time.
--

import Data.ByteString (ByteString)
import qualified Data.ByteString as P
-- import qualified Data.ByteString as L

import Data.List
import Data.Char
import Data.Word
import Data.Int

import System.Mem
import Control.Concurrent

import System.IO
import System.CPUTime
import System.IO.Unsafe
import Control.Monad
import Control.Exception
import Text.Printf

------------------------------------------------------------------------
-- a reference (incorrect, but fast) implementation:

import GHC.Ptr
import qualified GHC.Base as GHC
import qualified Data.ByteString      as B
import qualified Data.ByteString.Base as B

import Data.ByteString (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as C
import qualified Data.ByteString.Lazy   as L

import Data.List
import Data.Char
import Data.Word
import Data.Int

import System.IO
import Control.Monad
import Text.Printf

import qualified Codec.Binary.UTF8.String as UTF8

------------------------------------------------------------------------

main :: IO ()
main = do
    force (fps,chars,strs)
    printf "# Size of test data: %dk\n" ((floor $ (fromIntegral (B.length fps)) / 1024) :: Int)
    printf "#Char\t Optimal byteString decode\n"
    run 5 (fps,chars,strs) tests

--
-- Measure the difference building an decoded String from
-- bytestring+GHC's inbuilt decoder, and ours.
--
-- Most cost is in building the String.
--
tests =
    [ ("decode",
        [F ( app UTF8.decode)
        ,F ( app unpackCStringUTF8) ])

    , ("encode",
        [F ( app UTF8.encode) ])
    ]


-- unpackCStringUtf8# wants \0 termianted strings. rewrite it to take a
-- length instead, and we avoid the copy in useAsCString.
unpackCStringUTF8 :: B.ByteString -> [Char]
unpackCStringUTF8 b = unsafePerformIO $ B.unsafeUseAsCString b $ \(Ptr a) ->
    return (GHC.unpackCStringUtf8# a)


------------------------------------------------------------------------

run c x tests = sequence_ $ zipWith (doit c x) [1..] tests

doit :: Int -> a -> Int -> (String, [F a]) -> IO ()
doit count x n (s,ls) = do
    printf "%2d " n
    fn ls
    printf "\t# %-16s\n" (show s)
    hFlush stdout
  where fn xs = case xs of
                    [f,g]   -> runN count f x >> putStr "\n   "
                            >> runN count g x >> putStr "\t"
                    [f]     -> runN count f x >> putStr "\t"
                    _       -> return ()
        run f x = dirtyCache fps >> performGC >> threadDelay 100 >> time f x
        runN 0 f x = return ()
        runN c f x = run f x >> runN (c-1) f x

dirtyCache x = evaluate (P.foldl1' (+) x)
{-# NOINLINE dirtyCache #-}

time :: F a -> a -> IO ()
time (F f) a = do
    start <- getCPUTime
    v     <- force (f a)
    case v of
        B -> printf "--\t"
        _ -> do
            end   <- getCPUTime
            let diff = (fromIntegral (end - start)) / (10^12)
            printf "%0.3f  " (diff :: Double)
    hFlush stdout

------------------------------------------------------------------------
-- 
-- an existential list
--
data F a = forall b . Forceable b => F (a -> b)

data Result = T | B

--
-- a bit deepSeqish
--
class Forceable a where
    force :: a -> IO Result
    force v = v `seq` return T

#if !defined(HEAD)
instance Forceable P.ByteString where
    force v = P.length v `seq` return T
#endif

instance Forceable L.ByteString where
    force v = L.length v `seq` return T

-- instance Forceable SPS.PackedString where
--     force v = SPS.length v `seq` return T

-- instance Forceable PS.PackedString where
--     force v = PS.lengthPS v `seq` return T

instance Forceable a => Forceable (Maybe a) where
    force Nothing  = return T
    force (Just v) = force v `seq` return T

instance Forceable [a] where
    force v = length v `seq` return T

instance (Forceable a, Forceable b) => Forceable (a,b) where
    force (a,b) = force a >> force b

instance (Forceable a, Forceable b, Forceable c) => Forceable (a,b,c) where
    force (a,b,c) = force a >> force b >> force c

instance Forceable Int
instance Forceable Int64
instance Forceable Bool
instance Forceable Char
instance Forceable Word8
instance Forceable Ordering

-- used to signal undefinedness
instance Forceable () where force () = return B

------------------------------------------------------------------------
--
-- some large strings to play with
--

fps     :: P.ByteString
fps     = unsafePerformIO $ P.readFile dict
{-# NOINLINE fps #-}

chars   :: [Word8]
chars   = B.unpack fps
{-# NOINLINE chars #-}

strs    :: String
strs    = C.unpack fps
{-# NOINLINE strs #-}

dict  = "/usr/share/dict/words"

------------------------------------------------------------------------

type Input = (B.ByteString,[Word8],String)

class (Eq a, Ord a) => Ap a where app :: (a -> b) -> Input -> b

instance Ap B.ByteString where app f x = f (fst3 x)
instance Ap [Word8]      where app f x = f (snd3 x)
instance Ap String       where app f x = f (thd3 x)

fst3 (a,_,_) = a
snd3 (_,a,_) = a
thd3 (_,_,a) = a
