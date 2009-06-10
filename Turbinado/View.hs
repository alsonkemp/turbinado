module Turbinado.View (
        evalView,
        defaultContentType,
        -- limited export from Turbinado.View.Monad
        View,
        runView,
        -- * Functions
        liftIO, catch,
        insertView,
        insertDefaultView,
        insertComponent,
        prettyHtml,

        -- Module Exports
        module Turbinado.View.Helpers,
        module Turbinado.View.HTML,
        module Turbinado.Environment.CodeStore,
        module Turbinado.Environment.Params,
        module Turbinado.Environment.Request,
        module Turbinado.Environment.Response,
        module Turbinado.Environment.Settings,
        module Turbinado.Environment.Types,
        module Turbinado.Environment.ViewData
        ) where

import Control.OldException (catchDyn)
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans (MonadIO(..))
import Data.Char
import Data.List
import Data.Maybe
import qualified Network.HTTP as HTTP
import qualified Network.URI  as URI
import Prelude hiding (catch)
import System.FilePath
import Text.XHtml.Strict

import Turbinado.Controller.Monad hiding (catch)
import Turbinado.Environment.CodeStore
import Turbinado.Environment.Logger
import Turbinado.Environment.Params
import Turbinado.Environment.Request
import Turbinado.Environment.Response
import Turbinado.Environment.Settings
import Turbinado.Environment.Types
import Turbinado.Environment.ViewData
import Turbinado.Server.StandardResponse
import Turbinado.View.Exception
import Turbinado.View.Helpers
import Turbinado.View.HTML
import Turbinado.View.Monad hiding (liftIO)
import Turbinado.Utility.Data
import Turbinado.Utility.Naming
import qualified Config.Master as Config

evalView :: (HasEnvironment m) => VHtml -> m ()
evalView p = do e <- getEnvironment
                (x, e') <- liftIO $ runView p e
                pageResponse [] $ prettyHtml x

defaultContentType :: String
defaultContentType = "text/html; charset=ISO-8859-1"

insertDefaultView :: VHtml
insertDefaultView = 
             do cl <- getView
                debugM $ "    Layout: insertDefaultView : loading   " ++ (fst cl) ++ " - " ++ (snd cl)
                c <- retrieveCode CTView cl
                case c of
                  CodeLoadView       v _ -> v 
                  CodeLoadController _ _ -> error "retrieveAndRunLayout called, but returned CodeLoadController"
                  CodeLoadFailure    e     -> stringToVHtml e

insertView :: String -> String -> VHtml
insertView c a = 
             do debugM $ "    Layout: insertView : loading   " ++ c ++ " - " ++ a
                let converter = if Config.useLowerCasePaths
                                    then fromUnderscore
                                    else id
                    filename  = joinPath $ map normalise [converter c, converter a]
                c <- retrieveCode CTView (filename, "markup")
                case c of
                  CodeLoadView       v _ -> v 
                  CodeLoadController _ _ -> error "retrieveAndRunLayout called, but returned CodeLoadController"
                  CodeLoadFailure    e     -> stringToVHtml e
 
insertComponent :: String -> String -> [(String, String)] -> VHtml
insertComponent controller action opts =
           do debugM $ " insertComponent: Starting"
              p <- retrieveCode CTComponentController (joinPath [controller,"Controller"],  (toLower $ head action) : (tail action))
              case p of
                 CodeLoadMissing                    ->    stringToVHtml $ "insertComponent error: code missing : " ++ controller ++ " - " ++ action
                 CodeLoadFailure e                  ->    stringToVHtml $ "insertComponent error: " ++ e
                 CodeLoadComponentController p'   _ -> do oldE <- getEnvironment
                                                          mapM_ (\(k, v) -> setSetting k v) opts
                                                          p'
                                                          -- allow for overloading of the Component Controller and View
                                                          c <- getSetting "component-controller"
                                                          a <- getSetting "component-view"
                                                          insertComponentView oldE (fromMaybe controller c) (fromMaybe action a)
                 _                                  ->    stringToVHtml $ "insertComponent error: received incorrect CodeStatus"

insertComponentView :: Environment -> String -> String -> VHtml
insertComponentView oldE controller action =
           do debugM $ " insertComponentView: Starting"
              v  <- retrieveCode CTComponentView (joinPath [controller, "Views", action], "markup")
              case v of
                 CodeLoadMissing                    -> do setEnvironment oldE
                                                          stringToVHtml $ "insertComponentView error: code missing : " ++ (joinPath [controller, action]) ++ " - markup"
                 CodeLoadFailure e                  -> do setEnvironment oldE
                                                          stringToVHtml $ "insertComponentView error: " ++ e
                 CodeLoadComponentView v' _   -> do res <- v'
                                                    setEnvironment oldE
                                                    return res
                 _                            -> do setEnvironment oldE
                                                    stringToVHtml $ "insertComponentView error"

