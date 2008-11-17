module Turbinado.View.Helpers.Tags (
  anchorTag
  ) where 

import Turbinado.View

anchorTag :: String -> String -> View XML
anchorTag l t = <a href=l><% t %></a>


