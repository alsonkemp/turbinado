-- 
-- Copyright (c) 2005 Lemmih <lemmih@gmail.com>
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

module System.Plugins.LoadTypes
    ( Key (..)
    , Symbol
    , Type
    , Errors
    , PackageConf
    , Module (..)
    , ObjType (..)
    ) where

-- import Language.Hi.Parser

import HscTypes

data Key = Object String | Package String

type Symbol      = String
type Type        = String
type Errors      = [String]
type PackageConf = FilePath

data Module = Module { path  :: !FilePath
                     , mname :: !String
                     , kind  :: !ObjType
                     , iface :: ModIface    -- cache the iface
                     , key   :: Key
                     }

instance Ord Module where
    compare m1 m2 = mname m1 `compare` mname m2

instance Eq Module where
    m1 == m2 = mname m1 == mname m2

data ObjType = Vanilla | Shared deriving Eq
