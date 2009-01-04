
-- try to construct an orphan module == an instance decl-only module,
-- that uses classes and types not defined in this module

module C (C) where

import D

instance C a => D (T a) where

class C a where

