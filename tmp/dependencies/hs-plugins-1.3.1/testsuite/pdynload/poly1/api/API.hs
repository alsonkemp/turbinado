module API where

data Interface = Interface { 
        function :: (Num a) => a -> a -> a
}

plugin :: Interface
plugin = Interface  { function = error "no function defined" }

