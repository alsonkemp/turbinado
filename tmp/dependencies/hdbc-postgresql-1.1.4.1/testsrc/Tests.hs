{- arch-tag: Tests main file
Copyright (C) 2004-2005 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module Tests(tests) where
import Test.HUnit
import qualified Testbasics
import qualified TestSbasics
import qualified SpecificDBTests
import qualified TestMisc

test1 = TestCase ("x" @=? "x")

tests = TestList [TestLabel "test1" test1,
                  TestLabel "String basics" TestSbasics.tests,
                  TestLabel "SqlValue basics" Testbasics.tests,
                  TestLabel "SpecificDB" SpecificDBTests.tests,
                  TestLabel "Misc tests" TestMisc.tests]
