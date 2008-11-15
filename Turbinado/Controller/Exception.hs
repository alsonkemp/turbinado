{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Turbinado.Exception
-- Copyright   :  (c) Niklas Broberg 2008
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Niklas Broberg, nibro@cs.chalmers.se
-- Stability   :  experimental
-- Portability :  needs dynamic exceptions and deriving Typeable
--
-- Defines a datatype for runtime exceptions that may arise during
-- the evaluation of a Turbinado page.
-----------------------------------------------------------------------------
module Turbinado.Controller.Exception (
	Exception(..),
	throwController
	) where

import Data.Typeable
import Control.Exception (throwDyn)

data Exception
	=  ParameterLookupFailed String	-- ^ User tried to do an irrefutable parameter lookup
					-- that failed.
	-- | ... I'm sure there should be more exceptions, we'll add them when we get to them.
 deriving (Eq, Show, Typeable)

-- Internal funcion that throws a dynamic exception particular to Turbinado.
throwController :: Exception -> a
throwController = throwDyn

