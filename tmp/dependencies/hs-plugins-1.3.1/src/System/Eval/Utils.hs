-- 
-- Copyright (C) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- 
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
-- 
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
-- 
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
-- USA
-- 

--
-- compile and run haskell strings at runtime.
--

module System.Eval.Utils ( 

        Import,
        symbol,
        escape,
        getPaths,

        mkUniqueWith,
        cleanup,

        module Data.Maybe,
        module Control.Monad,

    ) where

import System.Plugins.Load                ( Symbol )
import System.Plugins.Utils

import System.IO
import System.Directory

import Data.Char

-- 
-- we export these so that eval() users have a nice time
--
import Data.Maybe 
import Control.Monad

--
-- imports Foo's
--
type Import = String

--
-- distinguished symbol name
--
symbol :: Symbol
symbol = "resource"

--
-- turn a Haskell string into a printable version of the same string
--
escape s = concatMap (\c -> showLitChar c $ "") s

--
-- For Dynamic eval's, work out the compile and load command lines
--
getPaths :: IO ([String],[String])
getPaths = do
        let make_line = ["-Onot","-fglasgow-exts","-package","plugins"]
        return (make_line,[])

-- ---------------------------------------------------------------------
-- create the tmp file, and write source into it, using wrapper to
-- create extra .hs src.
--
mkUniqueWith :: (String -> String -> [Import] -> String) 
             -> String 
             -> [Import] -> IO FilePath

mkUniqueWith wrapper src mods = do
        (tmpf,hdl) <- hMkUnique
        let nm   = mkModid (basename tmpf)       -- used as a module name
            src' = wrapper src nm mods
        hPutStr hdl src' >> hFlush hdl >> hClose hdl >> return tmpf

--
-- remove all the tmp files
--
cleanup :: String -> String -> IO ()
cleanup a b = mapM_ removeFile [a, b, replaceSuffix b ".hi"]

