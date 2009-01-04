--
-- API for plugin test
--

module API where

import Modules.Flags as Flags

data Interface = Interface {
        dbFunc :: Flags.FlagRec -> Int
}


plugin :: Interface
plugin = Interface { dbFunc = (\x -> 1) }

