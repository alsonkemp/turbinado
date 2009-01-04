module Plugin where

-- user doesn't import the API
-- and provides a polymorphic value

import API
resource :: Interface

--
-- should pass type check, and dump core
--
-- resource :: Num a => a
resource = 7
