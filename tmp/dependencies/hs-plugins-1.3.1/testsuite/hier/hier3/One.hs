
module One where

import qualified Two

resource = "This is the sub-plugin of (" ++ Two.resource ++ ")"

