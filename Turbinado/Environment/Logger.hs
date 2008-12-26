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


addLoggerToEnvironment :: (HasEnvironment m) => m ()
addLoggerToEnvironment = do e <- getEnvironment
                            f <- liftIO $ S.fileHandler "log" logLevel
                            liftIO $ L.updateGlobalLogger "Turbinado" ( L.setLevel logLevel . L.setHandlers [f])
                            mv <- liftIO $ newMVar ()
                            setEnvironment $ e {getLoggerLock = Just mv}

takeLoggerLock :: (HasEnvironment m) => m ()
takeLoggerLock = do e <- getEnvironment
                    liftIO $ takeMVar (fromJust $ getLoggerLock e)

putLoggerLock  :: (HasEnvironment m) => m ()
putLoggerLock =  do e <- getEnvironment
                    liftIO $ putMVar (fromJust $ getLoggerLock e) ()

wrapLoggerLock :: (HasEnvironment m) => (String -> IO ()) -> String -> m ()
wrapLoggerLock lf s = do takeLoggerLock
                         liftIO $ lf s
                         putLoggerLock

debugM :: (HasEnvironment m) => String -> m ()
debugM        = wrapLoggerLock (L.logM "Turbinado" L.DEBUG)
infoM :: (HasEnvironment m) => String -> m ()
infoM         = wrapLoggerLock (L.logM "Turbinado" L.INFO)
noticeM :: (HasEnvironment m) => String -> m ()
noticeM       = wrapLoggerLock (L.logM "Turbinado" L.NOTICE)
warningM :: (HasEnvironment m) => String -> m ()
warningM      = wrapLoggerLock (L.logM "Turbinado" L.WARNING)
errorM :: (HasEnvironment m) => String -> m ()
errorM        = wrapLoggerLock (L.logM "Turbinado" L.ERROR)
criticalM :: (HasEnvironment m) => String -> m ()
criticalM     = wrapLoggerLock (L.logM "Turbinado" L.CRITICAL)
alertM :: (HasEnvironment m) => String -> m ()
alertM        = wrapLoggerLock (L.logM "Turbinado" L.ALERT)
emergencyM :: (HasEnvironment m) => String -> m ()
emergencyM    = wrapLoggerLock (L.logM "Turbinado" L.EMERGENCY)
