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
-- Read information from a package.conf
--

module System.Plugins.Package {-everything-} where

type PackageName = String

--
-- Take directly from ghc/utils/ghc-pkg/Package.hs
--

data PackageConfig = Package {
	name            :: PackageName,
	auto		:: Bool,
	import_dirs     :: [FilePath],
	source_dirs     :: [FilePath],
	library_dirs    :: [FilePath],
	hs_libraries    :: [String],
	extra_libraries :: [String],
	include_dirs    :: [FilePath],
	c_includes      :: [String],
	package_deps    :: [String],
	extra_ghc_opts  :: [String],
	extra_cc_opts   :: [String],
	extra_ld_opts   :: [String],
	framework_dirs  :: [FilePath], -- ignored everywhere but on Darwin/MacOS X
	extra_frameworks:: [String]  -- ignored everywhere but on Darwin/MacOS X
     } deriving Show


defaultPackageConfig = Package {
	name = error "defaultPackage",
	auto = False,
	import_dirs     = [],
	source_dirs     = [],
	library_dirs    = [],
	hs_libraries    = [],
	extra_libraries = [],
	include_dirs    = [],
	c_includes      = [],
	package_deps    = [],
	extra_ghc_opts  = [],
	extra_cc_opts   = [],
	extra_ld_opts   = [],
	framework_dirs  = [],
	extra_frameworks= []
    }

