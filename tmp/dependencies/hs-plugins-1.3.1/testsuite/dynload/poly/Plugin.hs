module Plugin where

import API
import AltData.Dynamic

my_fun = plugin { 
                equals = \x y -> (x /= y)  -- a strange equals function :)
         }

resource_dyn :: Dynamic
resource_dyn = toDyn my_fun

