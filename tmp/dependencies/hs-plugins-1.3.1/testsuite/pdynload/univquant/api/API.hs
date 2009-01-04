module API where

data Interface = Interface { 
        function :: forall a. a -> a
}

plugin :: Interface
plugin = Interface  { function = id }

