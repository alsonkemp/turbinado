module Turbinado.Layout (
  insertView,
  styleSheet,
  javaScript,
  googleAnalytics
  ) where
import Control.Monad.State
import Control.Monad.Trans
import Data.Maybe
import Data.Dynamic
import Turbinado.Environment.Types
import Turbinado.Environment.CodeStore
import Turbinado.Environment.Logger
import Turbinado.Environment.Settings
import Turbinado.View

insertView :: View XML
insertView = do cl <- getView
                debugM $ "    Layout: insertView : loading   " ++ (fst cl) ++ " - " ++ (snd cl)
                c <- retrieveCode CTView cl
                case c of
                  CodeLoadView       v _ _ -> v 
                  CodeLoadController _ _ _ -> error "retrieveAndRunLayout called, but returned CodeLoadController"
                  CodeLoadFailure    e     -> return $ cdata e
                  
styleSheet :: String -> String -> View XML
styleSheet s m = return $ cdata $ "<link media=\"" ++ m ++"\" type=\"text/css\" rel=\"stylesheet\" href=\"/css/" ++ s ++".css\">"


javaScript :: String -> View XML
javaScript j = return $ cdata $ "<script type=\"text/javascript\" src=\"/js/" ++ j ++ ".js\"></script>"

googleAnalytics :: String -> View XML
googleAnalytics g = return $ cdata $ 
                    "<script type=\"text/javascript\"> " ++
                    "  var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\"); " ++
                    "  document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\")); " ++
                    "</script> " ++
                    "<script type=\"text/javascript\"> " ++
                    "  var pageTracker = _gat._getTracker(\"" ++ g ++ "\"); " ++
                    "  pageTracker._trackPageview(); " ++
                    "</script> "
