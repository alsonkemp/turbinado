{-# OPTIONS -fglasgow-exts #-}
-- 
-- Copyright (C) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- 
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
-- 

module System.Plugins.Parser ( 
        parse, mergeModules, pretty, parsePragmas,
        HsModule(..) ,
        replaceModName
  ) where

#include "../../../config.h"

import Data.List 
import Data.Char
import Data.Either

#if defined(WITH_HSX)
import Language.Haskell.Hsx
#else
import Language.Haskell.Parser
import Language.Haskell.Syntax
import Language.Haskell.Pretty
#endif

--
-- | parse a file (as a string) as Haskell src
-- 
parse :: FilePath                -- ^ module name
      -> String                  -- ^ haskell src
      -> Either String HsModule  -- ^ abstract syntax

parse f fsrc = 
#if defined(WITH_HSX)
    case parseFileContentsWithMode (ParseMode f) fsrc of
#else
    case parseModuleWithMode (ParseMode f) fsrc of
#endif
        ParseOk src       -> Right src
        ParseFailed loc _ -> Left $ srcmsg loc
  where
    srcmsg loc = "parse error in " ++ f ++ "\n" ++ 
                  "line: "  ++ (show $ srcLine loc) ++ 
                  ", col: " ++ (show $ srcColumn loc)++ "\n"

--
-- | pretty print haskell src
--
-- doesn't handle operators with '#' at the end. i.e. unsafeCoerce#
--
pretty :: HsModule -> String
pretty code = prettyPrintWithMode (defaultMode { linePragmas = True }) code


-- | mergeModules : generate a full Haskell src file, give a .hs config
-- file, and a stub to take default syntax and decls from. Mostly we
-- just ensure they don't do anything bad, and that the names are
-- correct for the module.
--
-- Transformations:
--
--      . Take src location pragmas from the conf file (1st file)
--      . Use the template's (2nd argument) module name
--      . Only use export list from template (2nd arg)
--      . Merge top-level decls
--      . need to force the type of the plugin to match the stub,
--      overwriting any type they supply.
--
mergeModules :: HsModule ->    -- Configure module
                HsModule ->    -- Template module
                HsModule       -- A merge of the two

mergeModules (HsModule l  _   _  is  ds )
             (HsModule _  m' es' is' ds')
         = (HsModule l  m' es' 
                        (mImps m' is is') 
                        (mDecl ds ds') )

-- 
-- | replace Module name with String.
--
replaceModName :: HsModule -> String -> HsModule
replaceModName (HsModule l _ es is ds) nm = (HsModule l (Module nm) es is ds)

--  
-- | merge import declarations:
--
--  *   ensure that the config file doesn't import the stub name
--  *   merge import lists uniquely, and when they match, merge their decls
--
-- TODO * we don't merge imports of the same module from both files. 
--      We should, and then merge the decls in their import list
--      * rename args, too confusing.
--
-- quick fix: strip all type signatures from the source.
--
mImps :: Module ->              -- plugin module name
        [HsImportDecl] ->       -- conf file imports
        [HsImportDecl] ->       -- stub file imports
        [HsImportDecl]

mImps plug_mod cimps timps = 
    case filter (!~ self) cimps of cimps' -> unionBy (=~) cimps' timps
  where 
    self = ( HsImportDecl undefined plug_mod undefined undefined undefined )

--
-- | merge top-level declarations
--
-- Remove decls found in template, using those from the config file.
-- Need to sort decls by types, then decls first, in both.
--
-- Could we write a pass to handle editor, foo :: String ?
-- We must keep the type from the template.
--
mDecl ds es = let ds' = filter (not.typeDecl) ds
              in sortBy decls $! unionBy (=~) ds' es
  where
    decls a b = compare (encoding a) (encoding b)

    typeDecl :: HsDecl -> Bool
    typeDecl (HsTypeSig _ _ _) = True
    typeDecl _ = False

    encoding :: HsDecl -> Int
    encoding d = case d of
           HsFunBind _        -> 1
           HsPatBind _ _ _ _  -> 1
           _                  -> 0

--
-- syntactic equality over the useful Haskell abstract syntax
-- this may be extended if we try to merge the files more thoroughly
--
class SynEq a where
    (=~) :: a -> a -> Bool
    (!~) :: a -> a -> Bool
    n !~ m = not (n =~ m)
        
instance SynEq HsDecl where
    (HsPatBind _ (HsPVar n) _ _) =~ (HsPatBind _ (HsPVar m) _ _) = n == m
    (HsTypeSig _ (n:_) _)        =~ (HsTypeSig _ (m:_) _)        = n == m
    _ =~ _ = False

instance SynEq HsImportDecl where
    (HsImportDecl _ m _ _ _) =~ (HsImportDecl _ n _ _ _)    = n == m


--
-- | Parsing option pragmas.
--
-- This is not a type checker. If the user supplies bogus options,
-- they'll get slightly mystical error messages. Also, we /want/ to
-- handle -package options, and other /static/ flags. This is more than
-- GHC.
--
-- GHC user's guide : 
--
-- >    OPTIONS pragmas are only looked for at the top of your source
-- >    files, up to the first (non-literate,non-empty) line not
-- >    containing OPTIONS. Multiple OPTIONS pragmas are recognised.
--
-- based on getOptionsFromSource(), in main\/DriverUtil.hs
--
parsePragmas :: String              -- ^ input src
            -> ([String],[String])  -- ^ normal options, global options

parsePragmas s = look $ lines s
    where
        look [] = ([],[])
        look (l':ls) =
            let l = remove_spaces l'
            in case () of
                () | null l                      -> look ls
                   | prefixMatch "#" l           -> look ls
                   | prefixMatch "{-# LINE" l    -> look ls
                   | Just (Option o) <- matchPragma l
                        -> let (as,bs) = look ls in (words o ++ as,bs)
                   | Just (Global g) <- matchPragma l
                        -> let (as,bs) = look ls in (as,words g ++ bs)
                   | otherwise -> ([],[])

--
-- based on main\/DriverUtil.hs
--
-- extended to handle dynamic options too
--

data Pragma = Option !String | Global !String

matchPragma :: String -> Maybe Pragma
matchPragma s
        | Just s1 <- maybePrefixMatch "{-#" s, -- -}
          Just s2 <- maybePrefixMatch "OPTIONS" (remove_spaces s1),
          Just s3 <- maybePrefixMatch "}-#" (reverse s2)
        = Just (Option (reverse s3))

        | Just s1 <- maybePrefixMatch "{-#" s, -- -}
          Just s2 <- maybePrefixMatch "GLOBALOPTIONS" (remove_spaces s1),
          Just s3 <- maybePrefixMatch "}-#" (reverse s2)
        = Just (Global (reverse s3))

        | otherwise
        = Nothing

remove_spaces :: String -> String
remove_spaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace

--
-- verbatim from utils\/Utils.lhs
--
prefixMatch :: Eq a => [a] -> [a] -> Bool
prefixMatch [] _str = True
prefixMatch _pat [] = False
prefixMatch (p:ps) (s:ss) | p == s    = prefixMatch ps ss
                          | otherwise = False

maybePrefixMatch :: String -> String -> Maybe String
maybePrefixMatch []    rest = Just rest
maybePrefixMatch (_:_) []   = Nothing
maybePrefixMatch (p:pat) (r:rest)
        | p == r    = maybePrefixMatch pat rest
        | otherwise = Nothing
