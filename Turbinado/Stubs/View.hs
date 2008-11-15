module Turbinado.Stubs.View (
  anchorTag,
  module Turbinado.Stubs.Common,
  module Turbinado.View
  ) where

import Turbinado.Stubs.Common
import Turbinado.View

anchorTag :: String -> String -> View XML
anchorTag l t = <a href=l> <% t %> </a>
