module StringProcessorAPI where

data Interface = Interface { 
        stringProcessor :: String -> String 
}

plugin :: Interface
plugin = Interface { stringProcessor = id }
