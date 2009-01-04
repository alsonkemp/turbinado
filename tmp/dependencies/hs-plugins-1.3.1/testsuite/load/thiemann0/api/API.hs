module API where

data Test = Test { 
                field :: String 
        }

test :: Test
test = Test { field = "default value" }
