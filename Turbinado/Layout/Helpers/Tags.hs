module Turbinado.Layout.Helpers.Tags (
  styleSheetTag
  ) where 

import Turbinado.View

styleSheetTag :: String -> String -> VHtml
styleSheetTag s m =
    itag "link" ! [strAttr "media" m, strAttr "type" "text/css", strAttr "rel" "stylesheet", strAttr "href" ("/css/" ++ s ++".css")]


