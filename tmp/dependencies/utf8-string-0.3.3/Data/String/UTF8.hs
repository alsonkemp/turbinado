{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fallow-undecidable-instances #-}
module Data.String.UTF8
  ( -- * Representation
    UTF8
  , UTF8Bytes()
  , fromString
  , toString
  , fromRep
  , toRep
  , G.replacement_char

  -- * Character based operations
  , uncons
  , splitAt
  , take
  , drop
  , span
  , break
  , foldl
  , foldr
  , length
  , lines
  , lines'

  -- * Representation based operations
  , null
  , decode
  , byteSplitAt
  , byteTake
  , byteDrop
  ) where

import Prelude hiding (null,take,drop,span,break
                      ,foldl,foldr,length,lines,splitAt)
import qualified Codec.Binary.UTF8.Generic as G
import Codec.Binary.UTF8.Generic (UTF8Bytes)

-- | The type of strngs that are represented using tthe UTF8 encoding.
-- The parameters is the type of the container for the representation.
newtype UTF8 string = Str string deriving (Eq,Ord)   -- XXX: Is this OK?

instance UTF8Bytes string index => Show (UTF8 string) where
  show x = show (toString x)

fromRep :: string -> UTF8 string
fromRep = Str

toRep :: UTF8 string -> string
toRep (Str x) = x

-- | Converts a Haskell string into a UTF8 encoded string.
-- Complexity: linear.
fromString :: UTF8Bytes string index => String -> UTF8 string
fromString xs = Str (G.fromString xs)

-- | Convert a UTF8 encoded string into a Haskell string.
-- Invalid characters are replaced by 'replacement_char'.
-- Complexity: linear.
toString :: UTF8Bytes string index => UTF8 string -> String
toString (Str xs) = G.toString xs

-- | Checks if there are no more bytes in the underlying representation.
null :: UTF8Bytes string index => UTF8 string -> Bool
null (Str x) = G.null x

-- | Split after a given number of characters.
-- Negative values are treated as if they are 0.
-- See also 'bytesSplitAt'.
splitAt :: UTF8Bytes string index
        => index -> UTF8 string -> (UTF8 string, UTF8 string)
splitAt x (Str bs)  = case G.splitAt x bs of
                        (s1,s2) -> (Str s1, Str s2)

-- | Split after a given number of bytes in the underlying representation.
-- See also 'splitAt'.
byteSplitAt :: UTF8Bytes string index
             => index -> UTF8 string -> (UTF8 string, UTF8 string)
byteSplitAt n (Str x) = case G.bsplit n x of
                          (as,bs) -> (Str as, Str bs)

-- | Take only the given number of bytes from the underlying representation.
-- See also 'take'.
byteTake :: UTF8Bytes string index => index -> UTF8 string -> UTF8 string
byteTake n (Str x) = Str (fst (G.bsplit n x))

-- | Drop the given number of bytes from the underlying representation.
-- See also 'drop'.
byteDrop :: UTF8Bytes string index => index -> UTF8 string -> UTF8 string
byteDrop n (Str x) = Str (G.bdrop n x)


-- | @take n s@ returns the first @n@ characters of @s@.
-- If @s@ has less then @n@ characters, then we return the whole of @s@.
take :: UTF8Bytes string index => index -> UTF8 string -> UTF8 string
take n (Str bs) = Str (G.take n bs)

-- | @drop n s@ returns the @s@ without its first @n@ characters.
-- If @s@ has less then @n@ characters, then we return the an empty string.
drop :: UTF8Bytes string index => index -> UTF8 string -> UTF8 string
drop n (Str bs) = Str (G.drop n bs)

-- | Split a string into two parts:  the first is the longest prefix
-- that contains only characters that satisfy the predicate; the second
-- part is the rest of the string.
-- Invalid characters are passed as '\0xFFFD' to the predicate.
span :: UTF8Bytes string index
     => (Char -> Bool) -> UTF8 string -> (UTF8 string, UTF8 string)
span p (Str bs) = case G.span p bs of
                    (s1,s2) -> (Str s1, Str s2)

-- | Split a string into two parts:  the first is the longest prefix
-- that contains only characters that do not satisfy the predicate; the second
-- part is the rest of the string.
-- Invalid characters are passed as 'replacement_char' to the predicate.
break :: UTF8Bytes string index
      => (Char -> Bool) -> UTF8 string -> (UTF8 string, UTF8 string)
break p (Str bs)  = case G.break p bs of
                      (s1,s2) -> (Str s1, Str s2)

-- | Get the first character of a byte string, if any.
-- Invalid characters are replaced by 'replacement_char'.
uncons :: UTF8Bytes string index
       => UTF8 string -> Maybe (Char, UTF8 string)
uncons (Str x)  = do (c,y) <- G.uncons x
                     return (c, Str y)

-- | Extract the first character for the underlying representation,
-- if one is avaialble.  It also returns the number of bytes used
-- in the representation of the character.
-- See also 'uncons', 'dropBytes'.
decode :: UTF8Bytes string index => UTF8 string -> Maybe (Char, index)
decode (Str x)  = G.decode x

-- | Traverse a bytestring (right biased).
foldr :: UTF8Bytes string index => (Char -> a -> a) -> a -> UTF8 string -> a
foldr cons nil (Str cs) = G.foldr cons nil cs

-- | Traverse a bytestring (left biased).
-- This fuction is strict in the accumulator.
foldl :: UTF8Bytes string index => (a -> Char -> a) -> a -> UTF8 string -> a
foldl add acc (Str cs)  = G.foldl add acc cs

-- | Counts the number of characters encoded in the bytestring.
-- Note that this includes replacment characters.
-- The function is linear in the number of bytes in the representation.
length :: UTF8Bytes string index => UTF8 string -> index
length (Str b) = G.length b

-- | Split a string into a list of lines.
-- Lines are termianted by '\n' or the end of the string.
-- Empty line may not be terminated by the end of the string.
-- See also 'lines\''.
lines :: UTF8Bytes string index => UTF8 string -> [UTF8 string]
lines (Str b) = map Str (G.lines b)   -- XXX: unnecessary map

-- | Split a string into a list of lines.
-- Lines are termianted by '\n' or the end of the string.
-- Empty line may not be terminated by the end of the string.
-- This function preserves the terminators.
-- See also 'lines'.
lines' :: UTF8Bytes string index => UTF8 string -> [UTF8 string]
lines' (Str x)  = map Str (G.lines' x)  -- XXX: unnecessary map

