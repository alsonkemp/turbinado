{- -*- mode: haskell; -*- 
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-}

{- PostgreSQL uses $1, $2, etc. instead of ? in query strings.  So we have to
do some basic parsing on these things to fix 'em up. -}

module Database.HDBC.PostgreSQL.Parser where

import Text.ParserCombinators.Parsec

escapeseq = (try $ string "''") <|>
            (try $ string "\\'")

literal = do char '\''
             s <- many (escapeseq <|> (noneOf "'" >>= (\x -> return [x])))
             char '\''
             return $ "'" ++ (concat s) ++ "'"

qidentifier = do char '"'
                 s <- many (noneOf "\"")
                 char '"'
                 return $ "\"" ++ s ++ "\""

comment = ccomment <|> linecomment

ccomment = do string "/*"
              c <- manyTill ((try ccomment) <|> 
                             (anyChar >>= (\x -> return [x])))
                   (try (string "*/"))
              return $ "/*" ++ concat c ++ "*/"

linecomment = do string "--"
                 c <- many (noneOf "\n")
                 char '\n'
                 return $ "--" ++ c ++ "\n"

-- FIXME: handle pgsql dollar-quoted constants

qmark = do char '?'
           n <- getState
           updateState (+1)
           return $ "$" ++ show n

statement = 
    do s <- many ((try qmark) <|>
                  (try comment) <|>
                  (try literal) <|>
                  (try qidentifier) <|>
                  (anyChar >>= (\x -> return [x])))
       return $ concat s

convertSQL :: String -> Either ParseError String
convertSQL input = runParser statement 1 "" input
