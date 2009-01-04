module Tiny ( resource, resource_dyn )  where

import API
import Data.Dynamic

resource = tiny { 

    field = "hello strange world"
    
}

resource_dyn :: Dynamic
resource_dyn = toDyn resource

