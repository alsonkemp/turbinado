module Turbinado.Server where

import Control.Concurrent
import Control.Monad

import System.IO
import System.Directory
import System.Time
import System.Environment (getArgs, getEnvironment)
import System.Console.GetOpt

import Network hiding (accept)
import Network.FastCGI
import Network.Socket
import Prelude hiding (catch)
import Data.Dynamic ( fromDynamic )
import Data.Maybe
import Data.Time
import Network.URI

import qualified Network.HTTP as HTTP
import qualified Network.URI as URI

import Config.App
import Config.Master

import Turbinado.Controller.Monad hiding (catch)
import Turbinado.Environment.Database
import Turbinado.Environment.Logger
import Turbinado.Environment.MimeTypes
import Turbinado.Environment.Request
import Turbinado.Environment.Response
import Turbinado.Environment.Routes
import Turbinado.Environment.Settings
import Turbinado.Environment.Types
import Turbinado.Environment.ViewData
import Turbinado.Environment.CodeStore (addCodeStoreToEnvironment)
import Turbinado.Server.Exception
import Turbinado.Server.ErrorHandler (handleCGIError, handleCGITurbinado, handleHTTPError, handleHTTPTurbinado)
import Turbinado.Server.RequestProcess (processRequest)
import Turbinado.Server.Network (receiveCGIRequest, sendCGIResponse, receiveHTTPRequest, sendHTTPResponse)
import Turbinado.Server.StandardResponse (addEmptyResponse, pageResponse)
import Turbinado.Server.StaticContent

data Flag 
     = UseHTTP Integer
     | UseCGI
     | UseFCGI
     | Help
    deriving (Show, Eq)

options :: [OptDescr Flag]
options =
    [ Option ['p']     ["port"] (ReqArg (UseHTTP . read) "PORTNUMBER") "start hsp runtime on port PORTNUMBER"
    , Option ['c']     ["eval"] (NoArg UseCGI)                      "run as a CGI app"
    , Option ['f']     ["eval"] (NoArg UseFCGI)                     "run as an FCGI app"
    , Option ['h','?'] ["help"] (NoArg Help)                        "show this message"
    ]

-- | Handle a few options, then kick off the server.
main :: IO ()
main =
    do args <- getArgs
       case getOpt Permute options args of
          ([UseHTTP n],[],[]) -> startServer (UseHTTP n)
          ([UseFCGI],[],[])   -> startServer (UseFCGI)
          ([UseCGI],[],[])    -> startServer (UseCGI)
          (opts,[],[])        -> putStr $ "ERROR: You may only specify one flag.\n\n" ++ usageInfo header options
          (_,_,errs)          -> ioError (userError (concat errs ++ usageInfo header options))
    where
     header = "Usage: turbinado [OPTION]"


-- | Starts the server, builds the basic 'Environment', builds the 'WorkerPool',
-- starts listening on the specified port.  As soon as a request is noticed, 
-- it's handed off to a 'WorkerThread' to be handled.  Lather, Rinse, Repeat.
startServer :: Flag -> IO ()
startServer using
    = withSocketsDo $ 
      do e <- runController 
               (sequence_ $ [ addLoggerToEnvironment
                            , addCodeStoreToEnvironment
                            , addMimeTypesToEnvironment "Config/mime.types"
                            , addRoutesToEnvironment
                            , addEmptyResponse
                            , addViewDataToEnvironment
                            , addSettingsToEnvironment
                            ]
                            ++ customSetupFilters
               ) 
               newEnvironment
         case using of 
           UseCGI     -> do processCGI (Network.FastCGI.runCGI) e
           UseFCGI    -> do processCGI (Network.FastCGI.runFastCGIConcurrent maxFCGIThreads) e
           UseHTTP p  -> do sock <- listenOn $ PortNumber $ fromIntegral p
                            workerPoolMVar <- newMVar $ WorkerPool 0 [] []
                            httpLoop sock e workerPoolMVar
           f -> error $ "Flag not supported: " ++ (show f)
    where
      httpLoop sock e workerPoolMVar = -- Run as Server
          do (sock', sockAddr) <- accept sock
             WorkerThread _ chan <- getWorkerThread workerPoolMVar e
             writeChan chan sock'
             httpLoop sock e workerPoolMVar


processCGI :: (CGI CGIResult -> IO ()) -> Environment -> IO ()
processCGI handler e = (handler $ 
                           do body <- getBody
                              hdrs <- getVars
                              uri  <- requestURI
                              method <- requestMethod
                              e' <- liftIO $ 
                                      runController (sequence_ [ addDatabaseToEnvironment
                                                               , receiveCGIRequest uri method body hdrs
                                                               , processRequest
                                                               ]
                                                    ) e
                              sendCGIResponse e'
                       ) `catchTurbinado` (\ex -> handleCGITurbinado ex e)
                          `catch` (\ex -> handleCGIError ex e)



------------------------------------------------
-- | Worker stuff used for HTTP processing
------------------------------------------------

-- | The basic loop for a 'WorkerThread': get the socket from the server mainloop,
-- receive a request, handle it, then put myself back into the 'WorkerPool'.
workerLoop :: MVar WorkerPool ->
              Environment ->
              Chan Socket ->
              IO ()
workerLoop workerPoolMVar e chan
         = do sock      <- readChan chan
              workerProcessRequest sock e
              putWorkerThread workerPoolMVar chan
              workerLoop workerPoolMVar e chan

-- | Basic request handling: setup the 'Environment' for this request,
-- run the real requestHandler, then ship the response back to the client.
workerProcessRequest :: Socket -> Environment -> IO ()
workerProcessRequest sock e
    = (do mytid <- myThreadId
          runController 
                  (do receiveHTTPRequest sock
                      tryStaticContent
                      processRequest
                      sendHTTPResponse sock
                  ) e
          return ()
      ) 
        `catchTurbinado` (\ex -> handleHTTPTurbinado sock ex e)
        `catch` (\ex -> handleHTTPError sock ex e)
        `finally` (sClose sock)


------------------------------------------------
-- | Worker Pool stuff
------------------------------------------------

-- | The 'WorkerPool' holds each idle or busy 'WorkerThread'.
-- When all 'WorkerThread's are busy, more are created by
-- 'getWorkerThread' and added to the 'WorkerPool'.
data WorkerPool = WorkerPool { numWorkers :: Int,
                               idleWorkers :: [WorkerThread],
                               busyWorkers :: [(WorkerThread, ExpiresTime)]}

-- | Each 'WorkerThread' has a 'ThreadId' and a 'Channel' for communication.
data WorkerThread = WorkerThread ThreadId (Chan Socket)

-- | 'ExpiresTime' is the time at which a 'WorkerThread' will be killed if it
-- has not completed its 'Request'.
type ExpiresTime = UTCTime

-- | 'getWorkerThread' returns a 'WorkerThread'.  If all threads are busy,
-- a new WorkerThread is created and returned.
getWorkerThread :: MVar WorkerPool -> Environment -> IO WorkerThread
getWorkerThread mv e = 
  do wp <- takeMVar mv
     case wp of
       WorkerPool n [] bs -> 
         do chan <- newChan
            e' <- runController (addDatabaseToEnvironment) e
            tid <- forkIO $ workerLoop mv e' chan
            let workerThread = WorkerThread tid chan 
            expiresTime <- getCurrentTime >>= \utct -> return $ addUTCTime (fromInteger stdTimeOut) utct
            putMVar mv $ WorkerPool (n+1) [] ((workerThread, expiresTime):bs)
            return workerThread
       WorkerPool n (idle:idles) busies ->
         do expiresTime <- getCurrentTime >>= \utct -> return $ addUTCTime (fromInteger stdTimeOut) utct
            putMVar mv $ WorkerPool n idles ((idle, expiresTime):busies)
            return idle

-- | 'putWorkerThread' puts a 'WorkerThread' back into the 'WorkerPool'.  This function
-- is used by the thread to put *itself* back into the pool.
putWorkerThread :: MVar WorkerPool -> Chan Socket -> IO () 
putWorkerThread mv chan = do
               WorkerPool n is bs <- takeMVar mv
               mytid <- myThreadId
               let bs' = filter (\(WorkerThread tid _, _) -> tid /= mytid) bs
               putMVar mv $ WorkerPool n ((WorkerThread mytid chan):is) bs'


stdTimeOut :: Integer
stdTimeOut = 90


