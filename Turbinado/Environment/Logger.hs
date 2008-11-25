module Turbinado.Environment.Logger where

import qualified System.Log.Logger as L
import qualified System.Log.Handler.Simple as S
import Control.Concurrent.MVar
import Control.Monad.State
import Control.Monad.Trans
import Turbinado.Environment.Types
import Config.Master
import Data.Dynamic
import Data.Maybe
import System.IO.Unsafe

import Turbinado.Controller.Monad

addLoggerToEnvironment :: Controller ()
addLoggerToEnvironment = do e <- get
                            f <- doIO $ S.fileHandler "log" logLevel
                            doIO $ L.updateGlobalLogger "Turbinado" ( L.setLevel logLevel . L.setHandlers [f])
                            mv <- doIO $ newMVar ()
                            put $ e {getLoggerLock = Just mv}

takeLoggerLock :: Controller ()
takeLoggerLock = do e <- get
                    doIO $ takeMVar (fromJust $ getLoggerLock e)

putLoggerLock  :: Controller ()
putLoggerLock =  do e <- get
                    doIO $ putMVar (fromJust $ getLoggerLock e) ()

wrapLoggerLock :: (String -> IO ()) -> String -> Controller ()
wrapLoggerLock lf s = do takeLoggerLock
                         doIO $ lf s
                         putLoggerLock

debugM        = wrapLoggerLock (L.logM "Turbinado" L.DEBUG)
infoM         = wrapLoggerLock (L.logM "Turbinado" L.INFO)
noticeM       = wrapLoggerLock (L.logM "Turbinado" L.NOTICE)
warningM      = wrapLoggerLock (L.logM "Turbinado" L.WARNING)
errorM        = wrapLoggerLock (L.logM "Turbinado" L.ERROR)
criticalM     = wrapLoggerLock (L.logM "Turbinado" L.CRITICAL)
alertM        = wrapLoggerLock (L.logM "Turbinado" L.ALERT)
emergencyM    = wrapLoggerLock (L.logM "Turbinado" L.EMERGENCY)
