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

module System.Plugins.Consts where

#include "../../../config.h"


#if __GLASGOW_HASKELL__ >= 604
import System.Directory          ( getTemporaryDirectory )
import System.IO.Unsafe          ( unsafePerformIO )
#endif


-- | path to *build* dir, used by eval() for testing the examples
top             = TOP

-- | what is ghc called?
ghc             = WITH_GHC

-- | path to standard ghc libraries
ghcLibraryPath  = GHC_LIB_PATH

-- | name of the system package.conf file
sysPkgConf = "package.conf"

-- | This code is from runtime_loader:
--   The extension used by system modules.
sysPkgSuffix = ".o"
objSuf       = sysPkgSuffix
hiSuf        = ".hi"
hsSuf        = ".hs"
#if defined(CYGWIN) || defined(__MINGW32__)
dllSuf       = ".dll"
#else
dllSuf       = ".so"
#endif

-- | The prefix used by system modules.  This, in conjunction with
--  'systemModuleExtension', will result in a module filename that looks
-- like \"HSconcurrent.o\"
sysPkgPrefix      = "HS"

-- | '_' on a.out, and Darwin
#if LEADING_UNDERSCORE == 1
prefixUnderscore        = "_" 
#else
prefixUnderscore        = ""
#endif

-- | Define tmpDir to where tmp files should be created on your platform

#if __GLASGOW_HASKELL__ >= 604
tmpDir = unsafePerformIO getTemporaryDirectory
{-# NOINLINE tmpDir #-}
#else
#if !defined(__MINGW32__)
tmpDir  = "/tmp"
#else
tmpDir  = error "tmpDir not defined for this platform. Try setting the TMPDIR env var"
#endif
#endif
