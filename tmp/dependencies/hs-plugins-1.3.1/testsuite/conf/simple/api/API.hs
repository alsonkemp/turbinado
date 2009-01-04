--
-- the configuration file interface.
--

module API where

data Color = Black | Grey | Green | Cyan | Yellow | Magenta | Red

data Interface = Interface {
        editor          :: IO String,
        attribution     :: String -> String,
        header_color    :: Color,
        colorize        :: [String],
        include         :: Bool
     }

-- Default settings
mail :: Interface
mail = Interface { 
        editor = return "vi",

        attribution = (\user -> user ++ " wrote:"),
        header_color = Grey,
        colorize = [],
        include = True
     }

