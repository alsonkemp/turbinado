module Parser.Common where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as T
import Data.Char
import Data.List
import Data.Maybe

indent p = take (sourceColumn (p) - 1) (repeat ' ')

manyTill1 :: CharParser () a -> CharParser () b -> CharParser () [a]
manyTill1 p e =  do ms <- manyTill p e
                    case (null ms) of
                      True  -> pzero
                      False -> return ms

tilEOL:: CharParser () String
tilEOL = manyTill1 (noneOf "\n") eol <?> "characters in tilEOL"

eol :: CharParser () Char
eol = newline <|> (eof >> return '\n')

blankLine :: CharParser () String
blankLine = char '\n' >> return ""

anyLine :: CharParser () String
anyLine = blankLine <|> tilEOL
