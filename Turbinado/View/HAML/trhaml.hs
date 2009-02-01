module Main where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as T
import Data.Char
import Data.List
import Data.Maybe
import System.IO.Unsafe

main = do s <- getContents 
          case (parse mainParser "stdin" s) of
            Left  err -> putStrLn "Error: " >> print err
            Right hs  -> putStrLn hs

-- Try to parse HAML, otherwise re-output raw lines

mainParser = do whiteSpace
                ls <- many1 (hamlCode <|> tilEOL)
                return $ unlines ls
--
-- * HAML lexer
--
hamlLexer = T.makeTokenParser emptyDef
whiteSpace= T.whiteSpace hamlLexer
lexeme    = T.lexeme hamlLexer
symbol    = T.symbol hamlLexer
natural   = T.natural hamlLexer
parens    = T.parens hamlLexer
semi      = T.semi hamlLexer
squares   = T.squares hamlLexer
stringLiteral= T.stringLiteral hamlLexer
identifier= T.identifier hamlLexer
reserved  = T.reserved hamlLexer
reservedOp= T.reservedOp hamlLexer
commaSep1 = T.commaSep1 hamlLexer
--
-- * Main HAML parsers
--

-- hamlCode is just many identifiers followed by = followed by a hamlBlock
-- f a b c = %somehaml
hamlCode = try ( do is <- many1 identifier
                    symbol "="
                    currentPos <- getPosition
                    x <- manyTill1 
                          (lexeme $ hamlBlock)
                          (notSameIndent currentPos)
                    return $ (concat $ intersperse " " is) ++ 
                             " = \n" ++
                             (concat $ (intersperse (indent currentPos ++ "+++\n")  $ filter (not . null) $ x))
                  )

-- A Block may start with some whitespace, then has a valid bit of data
hamlBlock   = do currentPos <- getPosition
                 bs <- manyTill1
                      (pTag <|> pText)
                      (notSameIndent currentPos)
                 return $ intercalate (indent currentPos ++ "+++\n") bs

pTag    = do    currentPos <- getPosition
                try
                    (do t  <- lexeme tagParser
                        ts <- (isInline currentPos >> char '/' >> return []) <|>
                              (hamlBlock) 
                        return $ intercalate "\n" $ filter (not . null) $
                          [ (indent currentPos) ++ "((" ++ (if (null ts) then "i" else "") ++ t  ++ ")"
                          , if null ts then [] else ts
                          , (indent currentPos) ++ ")\n"]
                    )

pText = lexeme stringParser

notSameIndent p = (eof >> return []) <|> 
                  (do innerPos <- getPosition
                      case (sourceColumn p) == (sourceColumn innerPos) of
                                True  -> pzero
                                False -> return []
                  )

--
-- * Various little parsers
--

tagParser :: CharParser () String
tagParser = do     t <- optionMaybe tagParser'
                   i <- optionMaybe idParser
                   c <- optionMaybe (many1 classParser)
                   a <- optionMaybe attributesParser
                   if (isJust t || isJust i || isJust c || isJust a)
                     then
                       do return $ "tag \"" ++ (fromMaybe "div" t) ++ "\"" ++
                           (if not (isJust i || isJust c || isJust a) then "" else
                              concat $ 
                               [ "!["
                               , intercalate ", " $ filter (not . null) 
                                   [ (maybe "" (\i' -> "strAttr \"id\" \"" ++ i' ++ "\"") i)
                                   , (maybe "" (\c' -> "strAttr \"class\" \"" ++ (intercalate " " c') ++ "\"") c)
                                   , (maybe "" (\kv -> intercalate ", " $ map (\(k,v) -> "strAttr \"" ++ k ++ "\" (" ++ v ++ ")") kv) a)
                                   ]
                               , "]"]
                           )
                     else pzero

tagParser' :: CharParser () String
tagParser' =  do char '%'
                 many1 termChar

idParser :: CharParser () String
idParser = do char '#'
              many1 termChar

classParser :: CharParser () String
classParser = do char '.'
                 many1 termChar

attributesParser :: CharParser () [(String, String)]
attributesParser = squares (commaSep1 attributeParser)

attributeParser :: CharParser () (String, String)
attributeParser = do k <- identifier
                     symbol "="
                     cs <- many1 identifier
                     return (k, intercalate " " cs)
                 
stringParser :: CharParser () String
stringParser = do   currentPos <- getPosition
                    modifier <- optionMaybe (char '=' <|> char '-')
                    whiteSpace
                    c <- alphaNum
                    cs<- tilEOL
                    case modifier of
                      Just '-' -> return $ (indent currentPos) ++ "-" ++ c:cs
                      Just '=' -> return $ (indent currentPos) ++ "(stringToHtml " ++ c:cs ++ ")"
                      Nothing  -> return $ (indent currentPos) ++ "(stringToHtml \"" ++ c:cs ++ "\")"
                      
                      
--
-- * Utility functions
--

isInline     p = do p2 <- getPosition
                    case (sourceLine p  ) == (sourceLine p2) of
                      True -> return []
                      False -> pzero
isSameIndent p1 p2 = (sourceColumn p1) == (sourceColumn p2)

tilEOL = manyTill1 (noneOf "\n") eol           
eol = newline <|> (eof >> return '\n')

termChar = satisfy (\c -> (isAlphaNum c) || (c `elem` termPunctuation) )
termPunctuation = "-_"
indent p = take (sourceColumn (p) - 1) (repeat ' ')

manyTill1 p e =  do ms <- manyTill p e
                    case (null ms) of
                      True  -> pzero
                      False -> return ms
