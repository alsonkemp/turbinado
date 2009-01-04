{-# OPTIONS -fglasgow-exts #-}
module Plugin where

import API
import AltData.Dynamic

v :: Int
v = 0xdeadbeef

resource_dyn :: Dynamic
resource_dyn = toDyn v

