module Parser.XHTML  where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as T
import Data.Char
import Data.List
import Data.Maybe

import Parser.Common

-- | XHTML begins with a valid tag
xhtmlParser = pTag

--
-- * XHTML lexer
--
xhtmlLexer = T.makeTokenParser emptyDef {
                 commentStart   = "<!--"
               , commentEnd     = "-->"
               , nestedComments = True
               , identStart     = letter <|> char '_'
               , identLetter    = alphaNum <|> oneOf "_'"
               , opStart        = opLetter emptyDef
               , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , caseSensitive  = False
             }
whiteSpace = T.whiteSpace xhtmlLexer
lexeme     = T.lexeme xhtmlLexer
symbol     = T.symbol xhtmlLexer
natural    = T.natural xhtmlLexer
parens     = T.parens xhtmlLexer
semi       = T.semi xhtmlLexer
braces     = T.braces xhtmlLexer
stringLiteral= T.stringLiteral xhtmlLexer
charLiteral= T.charLiteral xhtmlLexer
identifier = T.identifier xhtmlLexer
reserved   = T.reserved xhtmlLexer
reservedOp = T.reservedOp xhtmlLexer
commaSep1  = T.commaSep1 xhtmlLexer
--
-- * Main xhtml parsers
--

-- A Block always starts with some whitespace, then has an XHTML tag
xhtmlBlock :: CharParser () String
xhtmlBlock  = do whiteSpace
                 currentPos <- getPosition
                 bs <- (try pTag) <|> (try pPrintCode) <|> (try pCode) <|> pText
                 return  bs

pTag :: CharParser () String
pTag    = do    currentPos <- getPosition
                (t,ctag) <- lexeme tagParser <?> "tag"
                ts <- if (isNothing ctag) 
                          then return []
                          else (manyTill1 xhtmlBlock (fromJust ctag))
                return $ intercalate "\n" $ filter (not . null) $
                          [ (indent currentPos) ++ "((" ++ t  ++ ")"
                          , if (not $ null ts) then (indent currentPos ++ " (\n" ++ (intercalate "+++\n" ts ) ++ ")") else ""
                          , (indent currentPos) ++ " )"
                          ]
                <?> "tag for pTag"

pCode :: CharParser () String
pCode = lexeme codeParser

pPrintCode :: CharParser () String
pPrintCode = lexeme printCodeParser

pText :: CharParser () String
pText = lexeme stringParser <?> "text for pText"

--
-- * Various little parsers
--

-- | Parses a tag (e.g. <div id="smarfle">) and returns a View XHtml string
-- along with a parser for the closing tag (if needed).
tagParser :: CharParser () (String, Maybe (CharParser () String))
tagParser = do     (symbol "<" >> notFollowedBy (char '/') <?> "closing tag")
                   t <- lexeme tagParser'
                   as <- lexeme $ optionMaybe attributesParser
                   i  <- lexeme $ optionMaybe (symbol "/")
                   symbol ">"
                   return $ ((if (isNothing i) then "tag \"" else "itag \"") ++ t ++ "\"" ++
                              (if (isNothing as) || (null $ fromJust as) then "" else
                                concat $ 
                                 [ "!["
                                 , intercalate ", " $ map (\(k,v) -> "strAttr \"" ++ k ++ "\" " ++ v) $ fromJust as
                                 , "]"]
                             ),
                             if (isJust i) 
                              then Nothing 
                              else Just ((try (symbol "</" >> whiteSpace >> symbol t >> whiteSpace >> symbol ">")) 
                                          <?> ("closing XHTML tag '" ++ t ++ "'"))
                            )

tagParser' :: CharParser () String
tagParser' =  do many1 termChar <?> "valid tag character"

attributesParser :: CharParser () [(String, String)]
attributesParser = sepBy attributeParser whiteSpace

attributeParser :: CharParser () (String, String)
attributeParser = do k <- identifier <?> "identifier in key of attribute"
                     symbol "="
                     cs <- try (symbol "\"" >> manyTill1 anyChar (symbol "\"") >>= \v -> return $ "\"" ++ v ++ "\"") <|>
                               identifier
                     return (k, cs)
                 
stringParser :: CharParser () String
stringParser = do   currentPos <- getPosition
                    cs <- manyTill1 (noneOf "\n") (lookAhead (char '<') <|> eol)
                    return $ (indent currentPos) ++ "(stringToVHtml \"" ++ cs ++ "\")"
--                      Just '-' -> do b <- xhtmlBlock
--                                     return $ (indent currentPos) ++ "(" ++ cs ++ "\n" ++ b ++ "\n" ++ (indent currentPos) ++ ")"
                      
codeParser :: CharParser () String
codeParser = do currentPos <- getPosition
                symbol "<%"
                cs <- manyTill1 anyChar (symbol "%>")
                return $ (indent currentPos) ++ "(" ++ cs ++ ")"
 
printCodeParser :: CharParser () String
printCodeParser = do currentPos <- getPosition
                     symbol "<%="
                     cs <- manyTill1 anyChar (symbol "%>")
                     return $ (indent currentPos) ++ "(stringToVHtml  $ " ++ cs ++ ")"
                     
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

deeper p        = (eof >> return []) <|> 
                  (do innerPos <- getPosition
                      case (sourceColumn innerPos) > (sourceColumn p) of
                                True  -> return []
                                False -> pzero
                  )

