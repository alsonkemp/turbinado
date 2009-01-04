module Plugin1 where

import API
import Data.Char

resource = plugin {
        valueOf = map toUpper
}
