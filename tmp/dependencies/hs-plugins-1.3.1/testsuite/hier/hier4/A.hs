
-- now, the question is: is it possible to not depend on a module or
-- package, but nonetheless have an orphan to it? this could cause
-- problems....

module A where

import B

u :: Int
u = undefined
