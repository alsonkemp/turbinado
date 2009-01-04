
module API where

data Tiny = Tiny { field :: String }

tiny :: Tiny
tiny = Tiny { field = "default value" }

