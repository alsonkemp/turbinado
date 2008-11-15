module Turbinado.Layout (
  insertView,
  styleSheet,
  javaScript,
  googleAnalytics
  ) where

import Data.Maybe
import Data.Dynamic
import Turbinado.Environment
import Turbinado.Environment.CodeStore
import Turbinado.Environment.Logger
import Turbinado.Environment.Settings
import Turbinado.View

insertView :: View XML
insertView = do e <- getEnvironment
                let cs  = getCodeStore e
                    cl  = getView e
                doIO $ debugM e $ "    Layout: insertView : loading   " ++ (fst cl) ++ " - " ++ (snd cl)
                c <- doIO $ retrieveCode e CTView cl
                case c of
                  CodeLoadView       v _ -> v 
                  CodeLoadController _ _ -> error "retrieveAndRunLayout called, but returned CodeLoadController"
                  CodeLoadFailure        -> return $ cdata $ "CodeLoadFailure: insertView : " ++ (show $ fst $ getView e) ++ " - " ++ (show $ snd $ getView e)

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
                    "  pageTracker._trackViewview(); " ++
                    "</script> "
