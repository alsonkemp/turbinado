module Tiny ( resource )  where

import API

import A
import B
import C

resource = tiny { 

    field = a ++ b ++ c
    
}
