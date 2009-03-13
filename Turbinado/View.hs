module Turbinado.View (
        evalView,
        defaultContentType,
        -- limited export from Turbinado.View.Monad
        View, ViewT, ViewT',
        runView, runViewT,
        -- * Functions
        liftIO, catch,
        insertView,
        insertDefaultView,
        insertComponent,

        -- Module Exports
        module Turbinado.View.Helpers,
        module Turbinado.View.HTML,
        module Turbinado.View.XML,
        module Turbinado.View.XML.PCDATA,
        module Turbinado.View.XMLGenerator,
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
import Turbinado.View.XML hiding (Name)
import Turbinado.View.XML.PCDATA
import Turbinado.View.XMLGenerator
import Turbinado.Utility.General

evalView :: (HasEnvironment m) => View XML -> m ()
evalView p = do e <- getEnvironment
                (x, e') <- liftIO $ runView p e
                pageResponse [] $ renderAsHTML x

defaultContentType :: String
defaultContentType = "text/html; charset=ISO-8859-1"

insertDefaultView :: View XML
insertDefaultView = 
             do cl <- getView
                debugM $ "    Layout: insertDefaultView : loading   " ++ (fst cl) ++ " - " ++ (snd cl)
                c <- retrieveCode CTView cl
                case c of
                  CodeLoadView       v _ -> v 
                  CodeLoadController _ _ -> error "retrieveAndRunLayout called, but returned CodeLoadController"
                  CodeLoadFailure    e     -> return $ cdata e

insertView :: String -> String -> View XML
insertView c a = 
             do debugM $ "    Layout: insertView : loading   " ++ c ++ " - " ++ a
                c <- retrieveCode CTView (c, (toLower (head a)):(tail a))
                case c of
                  CodeLoadView       v _ -> v 
                  CodeLoadController _ _ -> error "retrieveAndRunLayout called, but returned CodeLoadController"
                  CodeLoadFailure    e     -> return $ cdata e
 
insertComponent :: String -> String -> [(String, String)] -> View XML
insertComponent controller action opts =
           do debugM $ " insertComponent: Starting"
              p <- retrieveCode CTComponentController (joinPath [controller,"Controller"],  (toLower $ head action) : (tail action))
              case p of
                 CodeLoadMissing                    ->    return $ cdata $ "insertComponent error: code missing : " ++ controller ++ " - " ++ action
                 CodeLoadFailure e                  ->    return $ cdata $ "insertComponent error: " ++ e
                 CodeLoadComponentController p'   _ -> do oldE <- getEnvironment
                                                          mapM_ (\(k, v) -> setSetting k v) opts
                                                          lift $ p'
                                                          -- allow for overloading of the Component Controller and View
                                                          c <- getSetting "component-controller"
                                                          a <- getSetting "component-view"
                                                          insertComponentView oldE (fromMaybe controller c) (fromMaybe action a)
                 _                                  ->    return $ cdata $ "insertComponent error: received incorrect CodeStatus"

insertComponentView :: Environment -> String -> String -> View XML
insertComponentView oldE controller action =
           do debugM $ " insertComponentView: Starting"
              v  <- retrieveCode CTComponentView (joinPath [controller, "Views", action], "markup")
              case v of
                 CodeLoadMissing                    -> do setEnvironment oldE
                                                          return $ cdata $ "insertComponentView error: code missing : " ++ (joinPath [controller, action]) ++ " - markup"
                 CodeLoadFailure e                  -> do setEnvironment oldE
                                                          return $ cdata $ "insertComponentView error: " ++ e
                 CodeLoadComponentView v' _   -> do res <- v'
                                                    setEnvironment oldE
                                                    return res
                 _                            -> do setEnvironment oldE
                                                    return $ cdata $ "insertComponentView error"

