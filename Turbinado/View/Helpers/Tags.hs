module Turbinado.View.Helpers.Tags (
  anchorTag,
  javaScriptFile,
  javaScriptBlock
  ) where 

import Turbinado.View.Monad
import Turbinado.View.XML
import Turbinado.View.XMLGenerator

anchorTag :: String -> String -> View XML
anchorTag l t = <a href=l><% t %></a>

javaScriptFile :: String -> View XML
javaScriptFile f = return $ cdata $ "<script type=\"text/javascript\" src=\"/js/" ++ f ++ ".js\"></script>"

javaScriptBlock :: String -> View XML
javaScriptBlock s = return $ cdata $ "<script type=\"text/javascript\">" ++ s ++ "</script>"




