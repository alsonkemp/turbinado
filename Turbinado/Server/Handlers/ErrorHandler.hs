module Turbinado.Server.Handlers.ErrorHandler where

import System.IO
import Prelude hiding (catch)
import Data.Dynamic ( fromDynamic )
import Network.Socket

import Turbinado.Environment
import Turbinado.Environment.Response
import Turbinado.Server.Exception
import Turbinado.Server.Network
import Turbinado.Server.StandardResponse

--import Turbinado.PrintDebug

handleError :: Socket -> Exception -> Environment -> IO ()
handleError s ex e = do e' <- errorResponse err e
                        sendResponse s e' 
               where err = unlines [ "Error in server: " ++ show ex
                                   ," please report as a bug to d00nibro@licia.dtek.chalmers.se"]


handleTurbinado :: Socket -> TurbinadoException -> Environment -> IO ()
handleTurbinado s he e = do
    case he of
        CompilationFailed errs     -> sendResponse s =<< (errorResponse err e) 
        	where err = unlines $ "File did not compile:" : errs
        FileNotFound file 	   -> sendResponse s =<< (fileNotFoundResponse file e)
        LoadApplicationFailed dir  -> sendResponse s =<< (errorResponse err e)
        	where err = "Failed to load application file in directory " ++ dir
        AppCompilationFailed errs  -> sendResponse s =<< (errorResponse err e)
        	where err = unlines $ "Application file did not compile:" : errs
        NoURISpecified		   -> sendResponse s =<< (badReqResponse e)
        TimedOut		   -> sendResponse s =<< (errorResponse err e)
        	where err = "Evaluation timed out"
        BadRequest _		   -> sendResponse s =<< (badReqResponse e)
        PageEvalFailed ex	   -> sendResponse s =<< (errorResponse err e)
         where err = "An exception occured during page evaluation\n:" ++
         	      case ex of
        		DynException dyn -> 
        	      	 case (fromDynamic dyn :: Maybe Exception) of
        	      	   Nothing   -> show ex
        	      	   Just hspe -> show hspe
        	      	_ -> show ex
