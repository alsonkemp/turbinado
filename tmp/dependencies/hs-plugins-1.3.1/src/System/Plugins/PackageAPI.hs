--
-- Copyright (C) 2005 Sean Seefried - http://www.cse.unsw.edu.au/~sseefried
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
-- We export an abstract interface to package conf`s because we have
-- to handle either traditional or Cabal style package conf`s.
--

module System.Plugins.PackageAPI (
         PackageName
       , PackageConfig
       , packageName
       , packageName_
       , importDirs
       , hsLibraries
       , libraryDirs
       , extraLibraries
       , ldOptions
       , packageDeps
       , updImportDirs
       , updLibraryDirs 
   ) where

#include "../../../config.h"

#if CABAL == 1 || __GLASGOW_HASKELL__ >= 604
import Distribution.InstalledPackageInfo
import Distribution.Package hiding (depends, packageName)
#else
import System.Plugins.Package
#endif

packageName    :: PackageConfig -> PackageName 
packageDeps    :: PackageConfig -> [PackageName]
updImportDirs  :: ([FilePath] -> [FilePath]) -> PackageConfig -> PackageConfig
updLibraryDirs :: ([FilePath] -> [FilePath]) -> PackageConfig -> PackageConfig

-- We use different package.conf parsers when running on 6.2.x or 6.4
#if CABAL == 1 || __GLASGOW_HASKELL__ >= 604

type PackageName = String

type PackageConfig = InstalledPackageInfo

packageName = showPackageId . package
packageName_ = pkgName . package
packageDeps = (map showPackageId) . depends

updImportDirs f pk@(InstalledPackageInfo { importDirs = idirs }) =
        pk { importDirs = f idirs }
updLibraryDirs f pk@(InstalledPackageInfo { libraryDirs = ldirs }) =
        pk { libraryDirs = f ldirs }
#else

packageName    = name
packageName_   = name
packageDeps    = package_deps

updImportDirs f pk@(Package {import_dirs = idirs}) 
        = pk {import_dirs = f idirs}

updLibraryDirs f pk@(Package {library_dirs = ldirs}) 
        = pk {library_dirs = f ldirs}

importDirs     :: PackageConfig -> [FilePath]
importDirs     = import_dirs

hsLibraries    :: PackageConfig -> [String]
hsLibraries    = hs_libraries

libraryDirs    :: PackageConfig -> [FilePath]
libraryDirs    = library_dirs

extraLibraries :: PackageConfig -> [String]
extraLibraries = extra_libraries

ldOptions :: PackageConfig -> [String]
ldOptions = extra_ld_opts

#endif
