module API where

data Interface = Interface {
       field :: String 
}

plugin :: Interface
plugin = Interface { field = undefined }
