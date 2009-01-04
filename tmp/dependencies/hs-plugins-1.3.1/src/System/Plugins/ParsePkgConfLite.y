-- 
-- Copyright (C) 2004 Sean Seefried - http://www.cse.unsw.edu.au/~sseefried
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
-- Taken (apart from the most minor of alterations) from 
-- ghc/utils/ghc-pkg/ParsePkgConfLite.hs:
--
-- (c) Copyright 2002, The University Court of the University of Glasgow. 
--

{

{-# OPTIONS -w #-}

module System.Plugins.ParsePkgConfLite ( 
        parsePkgConf, parseOnePkgConf
  ) where

import System.Plugins.Package  ( PackageConfig(..), defaultPackageConfig )

import Char             ( isSpace, isAlpha, isAlphaNum, isUpper )
import List             ( break )

}

%token
 '{'		{ ITocurly }
 '}'		{ ITccurly }
 '['		{ ITobrack }
 ']'		{ ITcbrack }
 ','		{ ITcomma }
 '='		{ ITequal }
 VARID   	{ ITvarid    $$ }
 CONID   	{ ITconid    $$ }
 STRING		{ ITstring   $$ }

%name parse pkgconf
%name parseOne pkg
%tokentype { Token }
%%

pkgconf :: { [ PackageConfig ] }
	: '[' ']'			{ [] }
	| '[' pkgs ']'			{ reverse $2 }

pkgs 	:: { [ PackageConfig ] }
	: pkg 				{ [ $1 ] }
	| pkgs ',' pkg			{ $3 : $1 }

pkg 	:: { PackageConfig }
	: CONID '{' fields '}'		{ $3 defaultPackageConfig }

fields  :: { PackageConfig -> PackageConfig }
	: field				{ \p -> $1 p }
	| fields ',' field		{ \p -> $1 ($3 p) }

field	:: { PackageConfig -> PackageConfig }
	: VARID '=' STRING		
                 {\p -> case $1 of
		   "name" -> p{name = $3}
		   _      -> error "unknown key in config file" }
			
        | VARID '=' bool
		{\p -> case $1 of {
		   	"auto" -> p{auto = $3};
		   	_      -> p } }

	| VARID '=' strlist		
		{\p -> case $1 of
		        "import_dirs"     -> p{import_dirs     = $3}
		        "library_dirs"    -> p{library_dirs    = $3}
		        "hs_libraries"    -> p{hs_libraries    = $3}
		        "extra_libraries" -> p{extra_libraries = $3}
		        "include_dirs"    -> p{include_dirs    = $3}
		        "c_includes"      -> p{c_includes      = $3}
		        "package_deps"    -> p{package_deps    = $3}
		        "extra_ghc_opts"  -> p{extra_ghc_opts  = $3}
		        "extra_cc_opts"   -> p{extra_cc_opts   = $3}
		        "extra_ld_opts"   -> p{extra_ld_opts   = $3}
		        "framework_dirs"  -> p{framework_dirs  = $3}
		        "extra_frameworks"-> p{extra_frameworks= $3}
			_other            -> p
		}

strlist :: { [String] }
        : '[' ']'			{ [] }
	| '[' strs ']'			{ reverse $2 }

strs	:: { [String] }
	: STRING			{ [ $1 ] }
	| strs ',' STRING		{ $3 : $1 }

bool    :: { Bool }
	: CONID				{% case $1 of {
					    "True"  -> True;
					    "False" -> False;
					    _       -> error ("unknown constructor in config file: " ++ $1) } }

{

data Token 
        = ITocurly
        | ITccurly
        | ITobrack
        | ITcbrack
        | ITcomma
        | ITequal
        | ITvarid String
        | ITconid String
        | ITstring String

lexer :: String -> [Token]

lexer [] = []
lexer ('{':cs) = ITocurly : lexer cs
lexer ('}':cs) = ITccurly : lexer cs
lexer ('[':cs) = ITobrack : lexer cs
lexer (']':cs) = ITcbrack : lexer cs
lexer (',':cs) = ITcomma : lexer cs
lexer ('=':cs) = ITequal : lexer cs
lexer ('"':cs) = lexString cs ""
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = lexID (c:cs) where
lexer _ = error "Unexpected token"

lexID cs = (if isUpper (head cs) then ITconid else ITvarid) id : lexer rest
    where
	(id,rest) = break (\c -> c /= '_' && not (isAlphaNum c)) cs

lexString ('"':cs) s = ITstring (reverse s) : lexer cs
lexString ('\\':c:cs) s = lexString cs (c:s)
lexString (c:cs) s = lexString cs (c:s)

happyError _ = error "Couldn't parse package configuration."

parsePkgConf :: String -> [PackageConfig]
parsePkgConf = parse . lexer

parseOnePkgConf :: String -> PackageConfig
parseOnePkgConf = parseOne . lexer

}
