--
-- Plugin
--

module Plugin where

import API
import Modules.Flags as Flags


resource = plugin {
        dbFunc = (\x -> Flags.f1 x)
}

