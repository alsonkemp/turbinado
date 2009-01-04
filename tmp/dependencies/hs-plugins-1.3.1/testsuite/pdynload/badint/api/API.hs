module API where

data Interface = Interface { 
                transform :: String -> String
     }

rsrc :: Interface
rsrc = Interface { transform = id }

