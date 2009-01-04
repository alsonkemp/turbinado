module Plugin where

import Data.Generics.Schemes

import API

resource = rsrc { 
     field = id listify
}
