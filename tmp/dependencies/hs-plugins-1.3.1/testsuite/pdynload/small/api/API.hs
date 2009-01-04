module API where

data Interface = Interface { 
        function :: String
}

plugin :: Interface
plugin = Interface  { function = "goodbye" }

