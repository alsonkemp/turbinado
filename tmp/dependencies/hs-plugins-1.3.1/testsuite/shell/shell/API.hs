module API where

-- the interface between the app and the plugin
data Interface = Interface { function :: String -> String }

-- default values for the interface
plugin :: Interface
plugin = Interface { function = id }
