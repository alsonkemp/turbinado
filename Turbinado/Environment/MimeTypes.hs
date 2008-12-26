-- -----------------------------------------------------------------------------
-- Copyright 2002, Simon Marlow.
-- Copyright 2006, Bjorn Bringert.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--  * Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
--
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
--  * Neither the name of the copyright holder(s) nor the names of
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- -----------------------------------------------------------------------------

module Turbinado.Environment.MimeTypes (
    setMimeTypes,
    mimeTypeOf,
    addMimeTypesToEnvironment
    ) where

import Data.List
import Data.Map (Map)
import Data.Typeable
import qualified Data.Map as Map hiding (Map)
import Text.ParserCombinators.Parsec
import Control.Monad.State
import Control.Monad.Trans

import Turbinado.Environment.Types

setMimeTypes :: (HasEnvironment m) => MimeTypes -> m ()
setMimeTypes mi = do e <- getEnvironment
                     setEnvironment $ e {getMimeTypes = Just mi}

addMimeTypesToEnvironment :: (HasEnvironment m) => FilePath -> m ()
addMimeTypesToEnvironment mime_types_file =
    do stuff <- liftIO $ readFile mime_types_file
       setMimeTypes (MimeTypes $ Map.fromList (parseMimeTypes stuff))


mimeTypeOf :: MimeTypes -> FilePath -> Maybe MimeType
mimeTypeOf (MimeTypes mime_types) filename =
    do let ext = extension filename
       if null ext
         then Nothing
         else Map.lookup ext mime_types

extension :: String -> String
extension fn = go (reverse fn) ""
  where go []      _   = ""
        go ('.':_) ext = ext
        go (x:s)   ext = go s (x:ext)

parseMimeTypes :: String -> [(String,MimeType)]
parseMimeTypes file =
  [ (ext,val)
  | Just (val,exts) <- map (parseMimeLine . takeWhile (/= '#')) (lines file)
  , ext <- exts
  ]

parseMimeLine :: String -> Maybe (MimeType, [String])
parseMimeLine l = case parse pMimeLine "MIME line" l of
                    Left _  -> Nothing
                    Right m -> Just m

pMimeLine :: Parser (MimeType, [String])
pMimeLine = do t <- pMimeType
               es <- (spaces >> sepBy pToken spaces)
               return (t, es)

pMimeType :: Parser MimeType
pMimeType = do part1 <- pToken
               char '/'
               part2 <- pToken
               return $ MimeType part1 part2

especials, tokenchar :: [Char]
especials = "()<>@,;:\\\"/[]?.="
tokenchar = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" \\ especials

pToken :: Parser String
pToken = many1 (oneOf tokenchar)

