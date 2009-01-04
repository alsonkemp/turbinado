module Plugin2 where

import API
import Data.Char

resource = plugin {
        valueOf = \s -> show $ map ord s
}

