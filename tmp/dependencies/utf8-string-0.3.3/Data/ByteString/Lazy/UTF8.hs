-- | This module provides fast, validated encoding and decoding functions
--   between 'ByteString's and 'String's. It does not exactly match the
--   output of the Codec.Binary.UTF8.String output for invalid encodings
--   as the number of replacement characters is sometimes longer.
module Data.ByteString.Lazy.UTF8
  ( B.ByteString
  , decode
  , replacement_char
  , uncons
  , splitAt
  , take
  , drop
  , span
  , break
  , fromString
  , toString
  , foldl
  , foldr
  , length
  , lines
  , lines'
  ) where

import Data.Bits
import Data.Word
import Data.Int
import qualified Data.ByteString.Lazy as B
import Prelude hiding (take,drop,splitAt,span,break,foldr,foldl,length,lines)

import Codec.Binary.UTF8.String(encode)

-- | Converts a Haskell string into a UTF8 encoded bytestring.
fromString :: String -> B.ByteString
fromString xs = B.pack (encode xs)

-- | Convert a UTF8 encoded bytestring into a Haskell string.
-- Invalid characters are replaced with '\xFFFD'.
toString :: B.ByteString -> String
toString bs = foldr (:) [] bs

-- | This character is used to mark errors in a UTF8 encoded string.
replacement_char :: Char
replacement_char = '\xfffd'

-- | Try to extract a character from a byte string.
-- Returns 'Nothing' if there are no more bytes in the byte string.
-- Otherwise, it returns a decoded character and the number of
-- bytes used in its representation.
-- Errors are replaced by character '\0xFFFD'.

-- XXX: Should we combine sequences of errors into a single replacement
-- character?
decode :: B.ByteString -> Maybe (Char,Int64)
decode bs = do (c,cs) <- B.uncons bs
               return (choose (fromEnum c) cs)
  where
  choose :: Int -> B.ByteString -> (Char, Int64)
  choose c cs
    | c < 0x80  = (toEnum $ fromEnum c, 1)
    | c < 0xc0  = (replacement_char, 1)
    | c < 0xe0  = bytes2 (mask c 0x1f) cs
    | c < 0xf0  = bytes3 (mask c 0x0f) cs
    | c < 0xf8  = bytes4 (mask c 0x07) cs
    | otherwise = (replacement_char, 1)

  mask :: Int -> Int -> Int
  mask c m = fromEnum (c .&. m)

  combine :: Int -> Word8 -> Int
  combine acc r = shiftL acc 6 .|. fromEnum (r .&. 0x3f)

  follower :: Int -> Word8 -> Maybe Int
  follower acc r | r .&. 0xc0 == 0x80 = Just (combine acc r)
  follower _ _                        = Nothing

  {-# INLINE get_follower #-}
  get_follower :: Int -> B.ByteString -> Maybe (Int, B.ByteString)
  get_follower acc cs = do (x,xs) <- B.uncons cs
                           acc1 <- follower acc x
                           return (acc1,xs)

  bytes2 :: Int -> B.ByteString -> (Char, Int64)
  bytes2 c cs = case get_follower c cs of
                  Just (d, _) | d >= 0x80  -> (toEnum d, 2)
                              | otherwise  -> (replacement_char, 1)
                  _ -> (replacement_char, 1)

  bytes3 :: Int -> B.ByteString -> (Char, Int64)
  bytes3 c cs =
    case get_follower c cs of
      Just (d1, cs1) ->
        case get_follower d1 cs1 of
          Just (d, _) | (d >= 0x800 && d < 0xd800) ||
                        (d > 0xdfff && d < 0xfffe) -> (toEnum d, 3)
                      | otherwise -> (replacement_char, 3)
          _ -> (replacement_char, 2)
      _ -> (replacement_char, 1)

  bytes4 :: Int -> B.ByteString -> (Char, Int64)
  bytes4 c cs =
    case get_follower c cs of
      Just (d1, cs1) ->
        case get_follower d1 cs1 of
          Just (d2, cs2) ->
            case get_follower d2 cs2 of
              Just (d,_) | d >= 0x10000 -> (toEnum d, 4)
                         | otherwise    -> (replacement_char, 4)
              _ -> (replacement_char, 3)
          _ -> (replacement_char, 2)
      _ -> (replacement_char, 1)


-- | Split after a given number of characters.
-- Negative values are treated as if they are 0.
splitAt :: Int64 -> B.ByteString -> (B.ByteString,B.ByteString)
splitAt x bs = loop 0 x bs
  where loop a n _ | n <= 0 = B.splitAt a bs
        loop a n bs1 = case decode bs1 of
                         Just (_,y) -> loop (a+y) (n-1) (B.drop y bs1)
                         Nothing    -> (bs, B.empty)

-- | @take n s@ returns the first @n@ characters of @s@.
-- If @s@ has less then @n@ characters, then we return the whole of @s@.
take :: Int64 -> B.ByteString -> B.ByteString
take n bs = fst (splitAt n bs)

-- | @drop n s@ returns the @s@ without its first @n@ characters.
-- If @s@ has less then @n@ characters, then we return the an empty string.
drop :: Int64 -> B.ByteString -> B.ByteString
drop n bs = snd (splitAt n bs)

-- | Split a string into two parts:  the first is the longest prefix
-- that contains only characters that satisfy the predicate; the second
-- part is the rest of the string.
-- Invalid characters are passed as '\0xFFFD' to the predicate.
span :: (Char -> Bool) -> B.ByteString -> (B.ByteString, B.ByteString)
span p bs = loop 0 bs
  where loop a cs = case decode cs of
                      Just (c,n) | p c -> loop (a+n) (B.drop n cs)
                      _ -> B.splitAt a bs

-- | Split a string into two parts:  the first is the longest prefix
-- that contains only characters that do not satisfy the predicate; the second
-- part is the rest of the string.
-- Invalid characters are passed as '\0xFFFD' to the predicate.
break :: (Char -> Bool) -> B.ByteString -> (B.ByteString, B.ByteString)
break p bs = span (not . p) bs

-- | Get the first character of a byte string, if any.
-- Malformed characters are replaced by '\0xFFFD'.
uncons :: B.ByteString -> Maybe (Char,B.ByteString)
uncons bs = do (c,n) <- decode bs
               return (c, B.drop n bs)

-- | Traverse a bytestring (right biased).
foldr :: (Char -> a -> a) -> a -> B.ByteString -> a
foldr cons nil cs = case uncons cs of
                      Just (a,as) -> cons a (foldr cons nil as)
                      Nothing     -> nil

-- | Traverse a bytestring (left biased).
-- This fuction is strict in the acumulator.
foldl :: (a -> Char -> a) -> a -> B.ByteString -> a
foldl add acc cs  = case uncons cs of
                      Just (a,as) -> let v = add acc a
                                     in seq v (foldl add v as)
                      Nothing     -> acc

-- | Counts the number of characters encoded in the bytestring.
-- Note that this includes replacment characters.
length :: B.ByteString -> Int
length b = loop 0 b
  where loop n xs = case decode xs of
                      Just (_,m) -> loop (n+1) (B.drop m xs)
                      Nothing -> n

-- | Split a string into a list of lines.
-- Lines are termianted by '\n' or the end of the string.
-- Empty line may not be terminated by the end of the string.
-- See also 'lines\''.
lines :: B.ByteString -> [B.ByteString]
lines bs | B.null bs  = []
lines bs = case B.elemIndex 10 bs of
             Just x -> let (xs,ys) = B.splitAt x bs
                       in xs : lines (B.tail ys)
             Nothing -> [bs]

-- | Split a string into a list of lines.
-- Lines are termianted by '\n' or the end of the string.
-- Empty line may not be terminated by the end of the string.
-- This function preserves the terminators.
-- See also 'lines'.
lines' :: B.ByteString -> [B.ByteString]
lines' bs | B.null bs  = []
lines' bs = case B.elemIndex 10 bs of
              Just x -> let (xs,ys) = B.splitAt (x+1) bs
                        in xs : lines' ys
              Nothing -> [bs]

