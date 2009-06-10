module Main where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as T
import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.IO.Unsafe

import Parser.HAML
import Parser.XHTML
import Parser.Id
import Parser.Common

main = do args <- getArgs
          case args of
            [origfile, infile, outfile] -> transformFile origfile infile outfile
            [infile, outfile] -> transformFile infile infile outfile
            [infile] -> readFile infile >>= parseTest parsers
            _ -> putStrLn "Incorrect usage... (fixme)"

transformFile o i ou = do
          s  <- readFile i
          case (parse parsers o s) of
            Left  err -> putStr "Error: " >> print err
            Right hs  -> writeFile ou $ unlines hs

parsers = manyTill1 
            (lexeme $ (try (whiteSpace >> ((try xhtmlParser) <|>  hamlParser))) 
                      <|> idParser)
            eof
