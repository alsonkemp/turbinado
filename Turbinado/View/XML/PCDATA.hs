-----------------------------------------------------------------------------
-- |
-- Module      :  HSP.XML.PCDATA
-- Copyright   :  (c) Niklas Broberg 2008
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Niklas Broberg, nibro@cs.chalmers.se
-- Stability   :  experimental
-- Portability :  Haskell 98
--
-- Escaping between CDATA <=> PCDATA
-----------------------------------------------------------------------------
module Turbinado.View.XML.PCDATA (
    escape  -- :: String -> String
  , unescape  -- :: String -> String
        , escaper
        , unescaper
        , xmlEscapeChars
  ) where


-- | Take a normal string and transform it to PCDATA by escaping special characters.
-- calls 'escaper' with 'xmlEscapeChars'
-- See also: 'escaper'
escape :: String -> String
escape = escaper xmlEscapeChars

-- | Take a normal string and transform it to PCDATA by escaping special characters.
-- See also: 'escape', 'xmlEscapeChars'
escaper :: [(Char, String)] -- ^ table of escape characters
        -> String -- ^ String to escape
        -> String -- ^ Escaped String
escaper _ [] = ""
escaper escapeChars (c:cs) = pChar escapeChars c ++ escaper escapeChars cs

pChar :: [(Char, String)] -- ^ table of escape characters
      -> Char -- ^ character to escape
      -> String -- ^ escaped character
pChar escapeChars c = 
    case lookup c escapeChars of
      Nothing -> [c]
      Just s  -> '&' : s ++ ";"

-- This list should be extended.
xmlEscapeChars :: [(Char, String)]
xmlEscapeChars = [
  ('&', "amp" ),
  -- ('\"',  "quot"  ),
  ('\'',  "apos"  ),
  ('<', "lt"  ),
  ('>', "gt"  )
  ]

-- | Take a PCDATA string and translate all escaped characters in it to the normal
-- characters they represent.
-- Does no error checking of input string, will fail if input is not valid PCDATA.
-- calls 'unescaper' with 'xmlEscapeChars'
-- See also: 'unescaper'
unescape :: String -> String
unescape = unescaper xmlEscapeChars

-- | Take a PCDATA string and translate all escaped characters in it to the normal
-- characters they represent.
-- Does no error checking of input string, will fail if input is not valid PCDATA.
-- See also: 'unescape', 'xmlEscapeChars'
unescaper :: [(Char, String)] -- ^ table of escape characters
          -> String -- ^ String to unescape
          -> String -- ^ unescaped String
unescaper escapeChars = reverse . unE ""
  where unE acc "" = acc
        unE acc (c:cs) = 
          case c of
            '&' -> let (esc, ';':rest) = break (==';') cs
                       Just ec = revLookup esc escapeChars
                   in unE (ec:acc) rest
            _ -> unE (c:acc) cs
    
revLookup e = lookup e . map (\(a,b) -> (b,a))
