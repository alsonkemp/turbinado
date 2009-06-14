module Turbinado.Server.ErrorHandler where

import System.IO
import Prelude hiding (catch)
import Data.Dynamic ( fromDynamic )
import Network.FastCGI
import Network.Socket

import Turbinado.Controller.Monad
import Turbinado.Environment.Types
import Turbinado.Environment.Response
import Turbinado.Server.Exception
import Turbinado.Server.Network
import Turbinado.Server.StandardResponse
import Turbinado.Utility.Data

handleHTTPError :: Socket -> Exception -> Environment -> IO ()
handleHTTPError s ex e = 
               do runController (errorResponse err >> sendHTTPResponse s) e
                  return ()
               where err = unlines [ "Error in server: " ++ show ex
                                   ," please report as a bug to alson@alsonkemp.com"]


handleCGIError :: Exception -> Environment -> IO ()
handleCGIError ex e = 
            do e' <- liftIO $ runController (errorResponse err) e
               runFastCGIorCGI $ sendCGIResponse e'
               where err = unlines [ "Error in server: " ++ show ex
                                   ," please report as a bug to alson@alsonkemp.com"]


handleHTTPTurbinado :: Socket -> TurbinadoException -> Environment -> IO ()
handleHTTPTurbinado s he e = do
    e' <- buildResponse he e
    runController (sendHTTPResponse s) e'
    return ()

handleCGITurbinado :: TurbinadoException -> Environment -> IO ()
handleCGITurbinado he e = do
    e' <- liftIO $ buildResponse he e
    runFastCGIorCGI $ sendCGIResponse e'

buildResponse he e = runController (
                        case he of
                           CompilationFailed errs    -> errorResponse err
                             where err = unlines $ "File did not compile:" : errs
                           FileNotFound file         -> fileNotFoundResponse file
                           LoadApplicationFailed dir -> errorResponse err
                             where err = "Failed to load application file in directory " ++ dir
                           AppCompilationFailed errs -> errorResponse err
                             where err = unlines $ "Application file did not compile:" : errs
                           NoURISpecified            -> badReqResponse
                           TimedOut                  -> errorResponse err
                             where err = "Evaluation timed out"
                           BadRequest _              -> badReqResponse
                           PageEvalFailed ex         -> errorResponse err
                             where err = "An exception occured during page evaluation\n:" ++
                                     case ex of
                                       DynException dyn -> 
                                         case (fromDynamic dyn :: Maybe Exception) of
                                           Nothing   -> show ex
                                           Just hspe -> show hspe
                                       _ -> show ex) e

