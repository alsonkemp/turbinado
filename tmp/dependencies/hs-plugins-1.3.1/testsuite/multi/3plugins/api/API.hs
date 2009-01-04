module API where

data Interface = Interface { 
        valueOf :: String -> String 
}

plugin :: Interface 
plugin = Interface { valueOf = id }

