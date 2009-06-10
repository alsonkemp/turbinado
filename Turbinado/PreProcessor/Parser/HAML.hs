module Parser.HAML (hamlParser) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as T
import Data.Char
import Data.List
import Data.Maybe

import Parser.Common

-- | HAML code starts with a HAML tag.
hamlParser = pTag

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
braces    = T.braces hamlLexer
stringLiteral= T.stringLiteral hamlLexer
identifier= T.identifier hamlLexer
reserved  = T.reserved hamlLexer
reservedOp= T.reservedOp hamlLexer
commaSep1 = T.commaSep1 hamlLexer
--
-- * Main HAML parsers
--
                          
-- A Block always starts with some whitespace, then has a valid bit of data
hamlBlock   = do whiteSpace
                 currentPos <- getPosition
                 bs <- (try pTag) <|> pText <?> "tag or text"
                 return bs

pTag    = do    currentPos <- getPosition
                t  <- lexeme tagParser <?> "tag"
                nc <- optionMaybe $ closeTag currentPos
                ts <- if (isJust nc)
                                then return []
                                else manyTill1 hamlBlock (shallowerOrEqual currentPos)
                return $ intercalate "\n" $ filter (not . null) $
                          [ (indent currentPos) ++ "((" ++ (if (null ts) then "i" else "") ++ t  ++ ")"
                          , if (not $ null ts) then (indent currentPos ++ " (\n" ++ (intercalate "+++\n" ts ) ++ ")") else ""
                          , (indent currentPos) ++ " )"]

pText = lexeme stringParser
closeTag p = isInline p >> char '/'

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
attributesParser = braces (commaSep1 attributeParser)

attributeParser :: CharParser () (String, String)
attributeParser = do k <- identifier <?> "identifier in key of attribute"
                     symbol "="
                     cs <- many1 identifier <?> "identifier in value of attribute"
                     return (k, intercalate " " cs)
                 
stringParser :: CharParser () String
stringParser = do   currentPos <- getPosition
                    modifier <- optionMaybe (char '=' <|> char '-')
                    whiteSpace
                    cs<- tilEOL
                    case modifier of
                      Just '-' -> do b <- hamlBlock
                                     return $ (indent currentPos) ++ "(" ++ cs ++ "\n" ++ b ++ "\n" ++ (indent currentPos) ++ ")"
                      Just '=' -> return $ (indent currentPos) ++ "(stringToVHtml $ " ++ cs ++ ")"
                      Nothing  -> return $ (indent currentPos) ++ "(stringToVHtml \"" ++ cs ++ "\")"
                      
                      
--
-- * Utility functions
--

isInline     p = do p2 <- getPosition
                    case (sourceLine p  ) == (sourceLine p2) of
                      True -> return []
                      False -> pzero
isSameIndent p1 p2 = (sourceColumn p1) == (sourceColumn p2)


termChar = satisfy (\c -> (isAlphaNum c) || (c `elem` termPunctuation) )
termPunctuation = "-_"


shallower p     = (eof >> return []) <|> 
                  (do innerPos <- getPosition
                      case (sourceColumn innerPos) < (sourceColumn p) of
                                True  -> return []
                                False -> pzero
                  )

shallowerOrEqual p = 
                  (eof >> return []) <|> 
                  (do innerPos <- getPosition
                      case (sourceColumn innerPos) <= (sourceColumn p) of
                                True  -> return []
                                False -> pzero
                  )

deeper p        = (eof >> return []) <|> 
                  (do innerPos <- getPosition
                      case (sourceColumn innerPos) > (sourceColumn p) of
                                True  -> return []
                                False -> pzero
                  )

