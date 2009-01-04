
-- P.Thiemann reports that 'import Char' leads to undefined symbol for
-- __stginit_Char_.

module Test where

import API
import Char

resource = test { field = map toUpper "success" }

