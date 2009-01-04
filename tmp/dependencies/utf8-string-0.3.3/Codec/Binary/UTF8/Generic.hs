{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
-- | This module provides fast, validated encoding and decoding functions
--   between 'ByteString's and 'String's. It does not exactly match the
--   output of the Codec.Binary.UTF8.String output for invalid encodings
--   as the number of replacement characters is sometimes longer.
module Codec.Binary.UTF8.Generic
  ( UTF8Bytes(..)
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
import Data.Int
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.List as List
import Prelude hiding (take,drop,splitAt,span,break,foldr,foldl,length,lines,null,tail)

import Codec.Binary.UTF8.String(encode)

class (Num s, Ord s) => UTF8Bytes b s | b -> s where
  bsplit        :: s -> b -> (b,b)
  bdrop         :: s -> b -> b
  buncons       :: b -> Maybe (Word8,b)
  elemIndex     :: Word8 -> b -> Maybe s
  empty         :: b
  null          :: b -> Bool
  pack          :: [Word8] -> b
  tail          :: b -> b

instance UTF8Bytes B.ByteString Int where
  bsplit        = B.splitAt
  bdrop         = B.drop
  buncons       = B.uncons
  elemIndex     = B.elemIndex
  empty         = B.empty
  null          = B.null
  pack          = B.pack
  tail          = B.tail

instance UTF8Bytes L.ByteString Int64 where
  bsplit        = L.splitAt
  bdrop         = L.drop
  buncons       = L.uncons
  elemIndex     = L.elemIndex
  empty         = L.empty
  null          = L.null
  pack          = L.pack
  tail          = L.tail

instance UTF8Bytes [Word8] Int where
  bsplit          = List.splitAt
  bdrop           = List.drop
  buncons (x:xs)  = Just (x,xs)
  buncons []      = Nothing
  elemIndex x xs  = List.elemIndex (toEnum (fromEnum x)) xs
  empty           = []
  null            = List.null
  pack            = id
  tail            = List.tail

-- | Converts a Haskell string into a UTF8 encoded bytestring.
{-# SPECIALIZE fromString :: String -> B.ByteString  #-}
{-# SPECIALIZE fromString :: String -> L.ByteString  #-}
{-# SPECIALIZE fromString :: String -> [Word8] #-}
fromString :: UTF8Bytes b s => String -> b
fromString xs = pack (encode xs)

-- | Convert a UTF8 encoded bytestring into a Haskell string.
-- Invalid characters are replaced with '\xFFFD'.
{-# SPECIALIZE toString :: B.ByteString -> String #-}
{-# SPECIALIZE toString :: L.ByteString -> String #-}
{-# SPECIALIZE toString :: [Word8] -> String #-}
toString :: UTF8Bytes b s => b -> String
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
{-# SPECIALIZE decode :: B.ByteString -> Maybe (Char,Int) #-}
{-# SPECIALIZE decode :: L.ByteString -> Maybe (Char,Int64) #-}
{-# SPECIALIZE decode :: [Word8]      -> Maybe (Char,Int) #-}
decode :: UTF8Bytes b s => b -> Maybe (Char,s)
decode bs = do (c,cs) <- buncons bs
               return (choose (fromEnum c) cs)
  where
  choose c cs
    | c < 0x80  = (toEnum $ fromEnum c, 1)
    | c < 0xc0  = (replacement_char, 1)
    | c < 0xe0  = bytes2 (mask c 0x1f) cs
    | c < 0xf0  = bytes3 (mask c 0x0f) cs
    | c < 0xf8  = bytes4 (mask c 0x07) cs
    | otherwise = (replacement_char, 1)

  mask c m = fromEnum (c .&. m)

  combine acc r = shiftL acc 6 .|. fromEnum (r .&. 0x3f)

  follower acc r | r .&. 0xc0 == 0x80 = Just (combine acc r)
  follower _ _                        = Nothing

  {-# INLINE get_follower #-}
  get_follower acc cs = do (x,xs) <- buncons cs
                           acc1 <- follower acc x
                           return (acc1,xs)

  bytes2 c cs = case get_follower c cs of
                  Just (d, _) | d >= 0x80  -> (toEnum d, 2)
                              | otherwise  -> (replacement_char, 1)
                  _ -> (replacement_char, 1)

  bytes3 c cs =
    case get_follower c cs of
      Just (d1, cs1) ->
        case get_follower d1 cs1 of
          Just (d, _) | (d >= 0x800 && d < 0xd800) ||
                        (d > 0xdfff && d < 0xfffe) -> (toEnum d, 3)
                      | otherwise -> (replacement_char, 3)
          _ -> (replacement_char, 2)
      _ -> (replacement_char, 1)

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
{-# SPECIALIZE splitAt :: Int   -> B.ByteString -> (B.ByteString,B.ByteString) #-}
{-# SPECIALIZE splitAt :: Int64 -> L.ByteString -> (L.ByteString,L.ByteString) #-}
{-# SPECIALIZE splitAt :: Int   -> [Word8] -> ([Word8],[Word8])    #-}
splitAt :: UTF8Bytes b s => s -> b -> (b,b)
splitAt x bs = loop 0 x bs
  where loop a n _ | n <= 0 = bsplit a bs
        loop a n bs1 = case decode bs1 of
                         Just (_,y) -> loop (a+y) (n-1) (bdrop y bs1)
                         Nothing    -> (bs, empty)

-- | @take n s@ returns the first @n@ characters of @s@.
-- If @s@ has less then @n@ characters, then we return the whole of @s@.
{-# INLINE take #-}
take :: UTF8Bytes b s => s -> b -> b
take n bs = fst (splitAt n bs)

-- | @drop n s@ returns the @s@ without its first @n@ characters.
-- If @s@ has less then @n@ characters, then we return the an empty string.
{-# INLINE drop #-}
drop :: UTF8Bytes b s => s -> b -> b
drop n bs = snd (splitAt n bs)

-- | Split a string into two parts:  the first is the longest prefix
-- that contains only characters that satisfy the predicate; the second
-- part is the rest of the string.
-- Invalid characters are passed as '\0xFFFD' to the predicate.
{-# SPECIALIZE span :: (Char -> Bool) -> B.ByteString -> (B.ByteString,B.ByteString) #-}
{-# SPECIALIZE span :: (Char -> Bool) -> L.ByteString -> (L.ByteString,L.ByteString) #-}
{-# SPECIALIZE span :: (Char -> Bool) -> [Word8] -> ([Word8],[Word8])    #-}
span :: UTF8Bytes b s => (Char -> Bool) -> b -> (b,b)
span p bs = loop 0 bs
  where loop a cs = case decode cs of
                      Just (c,n) | p c -> loop (a+n) (bdrop n cs)
                      _ -> bsplit a bs

-- | Split a string into two parts:  the first is the longest prefix
-- that contains only characters that do not satisfy the predicate; the second
-- part is the rest of the string.
-- Invalid characters are passed as '\0xFFFD' to the predicate.
{-# INLINE break #-}
break :: UTF8Bytes b s => (Char -> Bool) -> b -> (b,b)
break p bs = span (not . p) bs

-- | Get the first character of a byte string, if any.
-- Malformed characters are replaced by '\0xFFFD'.
{-# INLINE uncons #-}
uncons :: UTF8Bytes b s => b -> Maybe (Char,b)
uncons bs = do (c,n) <- decode bs
               return (c, bdrop n bs)

-- | Traverse a bytestring (right biased).
{-# SPECIALIZE foldr :: (Char -> a -> a) -> a -> B.ByteString -> a #-}
{-# SPECIALIZE foldr :: (Char -> a -> a) -> a -> L.ByteString -> a #-}
{-# SPECIALIZE foldr :: (Char -> a -> a) -> a -> [Word8]      -> a #-}
foldr :: UTF8Bytes b s => (Char -> a -> a) -> a -> b -> a
foldr cons nil cs = case uncons cs of
                      Just (a,as) -> cons a (foldr cons nil as)
                      Nothing     -> nil

-- | Traverse a bytestring (left biased).
-- This fuction is strict in the acumulator.
{-# SPECIALIZE foldl :: (a -> Char -> a) -> a -> B.ByteString -> a #-}
{-# SPECIALIZE foldl :: (a -> Char -> a) -> a -> L.ByteString -> a #-}
{-# SPECIALIZE foldl :: (a -> Char -> a) -> a -> [Word8]      -> a #-}
foldl :: UTF8Bytes b s => (a -> Char -> a) -> a -> b -> a
foldl add acc cs  = case uncons cs of
                      Just (a,as) -> let v = add acc a
                                     in seq v (foldl add v as)
                      Nothing     -> acc

-- | Counts the number of characters encoded in the bytestring.
-- Note that this includes replacment characters.
{-# SPECIALIZE length :: B.ByteString -> Int #-}
{-# SPECIALIZE length :: L.ByteString -> Int64 #-}
{-# SPECIALIZE length :: [Word8]      -> Int #-}
length :: UTF8Bytes b s => b -> s
length b = loop 0 b
  where loop n xs = case decode xs of
                      Just (_,m) -> loop (n+1) (bdrop m xs)
                      Nothing -> n

-- | Split a string into a list of lines.
-- Lines are termianted by '\n' or the end of the string.
-- Empty line may not be terminated by the end of the string.
-- See also 'lines\''.
{-# SPECIALIZE lines :: B.ByteString -> [B.ByteString] #-}
{-# SPECIALIZE lines :: L.ByteString -> [L.ByteString] #-}
{-# SPECIALIZE lines :: [Word8]      -> [[Word8]]       #-}
lines :: UTF8Bytes b s => b -> [b]
lines bs | null bs  = []
lines bs = case elemIndex 10 bs of
             Just x -> let (xs,ys) = bsplit x bs
                       in xs : lines (tail ys)
             Nothing -> [bs]

-- | Split a string into a list of lines.
-- Lines are termianted by '\n' or the end of the string.
-- Empty line may not be terminated by the end of the string.
-- This function preserves the terminators.
-- See also 'lines'.
{-# SPECIALIZE lines' :: B.ByteString -> [B.ByteString] #-}
{-# SPECIALIZE lines' :: L.ByteString -> [L.ByteString] #-}
{-# SPECIALIZE lines' :: [Word8]      -> [[Word8]]      #-}
lines' :: UTF8Bytes b s => b -> [b]
lines' bs | null bs  = []
lines' bs = case elemIndex 10 bs of
              Just x -> let (xs,ys) = bsplit (x+1) bs
                        in xs : lines' ys
              Nothing -> [bs]

