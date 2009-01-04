module Plugin where

import API

resource = plugin { function = (+) :: Int -> Int -> Int }
