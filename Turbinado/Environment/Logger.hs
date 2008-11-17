module Turbinado.Environment.Logger where

import qualified System.Log.Logger as L
import qualified System.Log.Handler.Simple as S
import Control.Concurrent.MVar
import Turbinado.Environment
import Config.Master
import Data.Dynamic

addLoggerToEnvironment :: EnvironmentFilter
addLoggerToEnvironment e = do f <- S.fileHandler "log" logLevel
                              L.updateGlobalLogger "Turbinado" ( L.setLevel logLevel . L.setHandlers [f])
                              mv <- newMVar ()
                              setLoggerLock mv e

loggerKey = "logger"

getLoggerLock :: Environment -> MVar ()
getLoggerLock = getKey loggerKey

setLoggerLock :: MVar () -> EnvironmentFilter
setLoggerLock l = setKey loggerKey l

takeLoggerLock :: Environment -> IO ()
takeLoggerLock e = takeMVar (getLoggerLock e)

putLoggerLock :: Environment -> IO ()
putLoggerLock e =  putMVar (getLoggerLock e) ()

wrapLoggerLock :: (String -> IO ()) -> Environment -> String -> IO ()
wrapLoggerLock lf e s = do takeLoggerLock e
                           lf s
                           putLoggerLock e

debugM        = wrapLoggerLock (L.logM "Turbinado" L.DEBUG)
infoM         = wrapLoggerLock (L.logM "Turbinado" L.INFO)
noticeM       = wrapLoggerLock (L.logM "Turbinado" L.NOTICE)
warningM      = wrapLoggerLock (L.logM "Turbinado" L.WARNING)
errorM        = wrapLoggerLock (L.logM "Turbinado" L.ERROR)
criticalM     = wrapLoggerLock (L.logM "Turbinado" L.CRITICAL)
alertM        = wrapLoggerLock (L.logM "Turbinado" L.ALERT)
emergencyM    = wrapLoggerLock (L.logM "Turbinado" L.EMERGENCY)
