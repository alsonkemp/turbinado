{-# OPTIONS -fglasgow-exts #-}
module Plugin where

import API
import AltData.Dynamic

my_fun = plugin { function = "plugin says \"hello\"" }

resource_dyn :: Dynamic
resource_dyn = toDyn my_fun

