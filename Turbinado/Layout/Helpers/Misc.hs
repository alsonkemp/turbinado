module Turbinado.Layout.Helpers.Misc (
  googleAnalytics
  ) where 

import Turbinado.View

googleAnalytics :: String -> View XML
googleAnalytics g = javaScriptBlock $ 
                    "  var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\"); " ++
                    "  document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\")); " ++
                    "  var pageTracker = _gat._getTracker(\"" ++ g ++ "\"); " ++
                    "  pageTracker._trackPageview(); "
