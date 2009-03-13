module Turbinado.Layout.Helpers.Tags (
  styleSheetTag
  ) where 

import Turbinado.View

styleSheetTag :: String -> String -> View XML
styleSheetTag s m = return $ cdata $ "<link media=\"" ++ m ++"\" type=\"text/css\" rel=\"stylesheet\" href=\"/css/" ++ s ++".css\">"


