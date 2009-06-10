module Parser.Id (idParser) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as T
import Data.Char
import Data.List
import Data.Maybe
import Parser.Common

-- | Returns all lines unmodified
idParser =  do currentPos <- getPosition
               l <- anyLine
               return $ (indent currentPos) ++ l
