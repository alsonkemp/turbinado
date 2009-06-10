module Turbinado.View.Helpers.Tags (
  anchorTag,
  javaScriptFile,
  javaScriptBlock
  ) where 

import Turbinado.View.Monad
import Turbinado.View.HTML

anchorTag :: String -> String -> VHtml
anchorTag l t = (tag "a"![strAttr "href" l]) (stringToVHtml t)

javaScriptFile :: String -> VHtml
javaScriptFile f = itag "script"![strAttr "type" "text/javascript", strAttr "src" $ "/js/" ++ f ++ ".js"]

javaScriptBlock :: String -> VHtml
javaScriptBlock s = (tag "script"![strAttr "type" "text/javascript"])(stringToVHtml s) 




