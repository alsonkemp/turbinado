module KeyValueParser where

import Text.ParserCombinators.Parsec

parseKeyValue = do
  key <- parseKey
  char '='
  value <- parseValue
  return (key, value)

parseKey = many1 letter

parseValue =
  do
    openQuote <- char '"' <|> char '\''
    value <- many1 letter
    char openQuote
    return value
  <|>
  do
    value <- many1 letter
    return value

parseString s = do
  case (parse parseKeyValue "" s) of
    Left err -> return ("Error " ++ show err)
    Right (key, value) -> return ("Key: " ++ key ++ ", Value: " ++ value)

resource :: String -> IO String
resource text = do
  parsedText <- mapM parseString (lines text)
  return (unlines parsedText)

