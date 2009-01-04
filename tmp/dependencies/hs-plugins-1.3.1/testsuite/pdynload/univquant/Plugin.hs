module Plugin where

import API

resource = plugin { function = my_id }

my_id :: forall a. a -> a
my_id x = x
