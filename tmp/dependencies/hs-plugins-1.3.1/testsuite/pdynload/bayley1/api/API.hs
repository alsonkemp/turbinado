module API where

type PluginAPI = Int -> String
action :: PluginAPI
action i = show i

