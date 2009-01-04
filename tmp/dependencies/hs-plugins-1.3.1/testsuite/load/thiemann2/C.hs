module C where

import API
import qualified A

resource = let Test s = A.resource in Test { field = s }
