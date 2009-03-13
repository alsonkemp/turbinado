module Main where

import Control.Concurrent
import Control.Monad

import System.IO
import System.Directory
import System.Time
import System.Environment (getArgs)
import System.Console.GetOpt

import Network hiding (accept)
import Network.Socket
import Prelude hiding (catch)
import Data.Dynamic ( fromDynamic )
import Data.Maybe
import Data.Time
import Network.URI

import qualified Network.HTTP as HTTP
import qualified Network.URI as URI

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
import Turbinado.Server.ErrorHandler (handleError, handleTurbinado)
import Turbinado.Server.RequestProcess (processRequest)
import Turbinado.Server.Network (receiveRequest, sendResponse)
import Turbinado.Server.StandardResponse (addEmptyResponse, pageResponse)
import Turbinado.Server.StaticContent

data Flag 
     = Port Integer
     | UseCGI
     | Help
    deriving (Show, Eq)

options :: [OptDescr Flag]
options =
    [ Option ['p']     ["port"] (ReqArg (Port . read) "PORTNUMBER") "start hsp runtime on port PORTNUMBER"
    , Option ['c']     ["eval"] (NoArg UseCGI)                "run as a CGI app"
    , Option ['h','?'] ["help"] (NoArg Help)                        "show this message"
    ]

-- | Handle a few options, then kick off the server.
main :: IO ()
main =
    do args <- getArgs
       case getOpt Permute options args of
          ([Port n],[],[]) -> startServer (fromIntegral n)
          ([UseCGI],[],[]) -> startServer runAsCGIPort
          (opts,[],[]) | Help `elem` opts -> putStr $ usageInfo header options
          (_,_,errs)       -> ioError (userError (concat errs ++ usageInfo header options))
    where
     header = "Usage: turbinado [OPTION]"


runAsCGIPort :: PortNumber
runAsCGIPort = fromIntegral 0

-- | Starts the server, builds the basic 'Environment', builds the 'WorkerPool',
-- starts listening on the specified port.  As soon as a request is noticed, 
-- it's handed off to a 'WorkerThread' to be handled.  Lather, Rinse, Repeat.
startServer :: PortNumber -> IO ()
startServer pnr
    = withSocketsDo $ 
      do e <- runController 
               (sequence_ $ [ addLoggerToEnvironment
                            , addCodeStoreToEnvironment
                            , addMimeTypesToEnvironment "Config/mime.types"
                            , addRoutesToEnvironment
                            ]
                            ++ customSetupFilters
               ) 
               newEnvironment
         case (pnr == runAsCGIPort) of 
           True  -> do mainLoop Nothing e Nothing
           False -> do sock <- listenOn $ PortNumber pnr
                       workerPoolMVar <- newMVar $ WorkerPool 0 [] []
                       mainLoop (Just sock) e (Just workerPoolMVar)
    where
      --mainLoop :: Socket -> WorkerPool -> IO ()
      mainLoop Nothing  e   Nothing               = -- Run as Server
          do e' <- runController (addDatabaseToEnvironment) e  -- need to add DB for this request (rather than per thread)
             workerLoop Nothing e' Nothing
      mainLoop (Just sock) e (Just workerPoolMVar) = -- Run as Server
          do (sock', sockAddr) <- accept sock
             WorkerThread _ chan <- getWorkerThread workerPoolMVar e
             writeChan chan sock'
             mainLoop (Just sock) e (Just workerPoolMVar)


------------------------------------------------
-- | Worker stuff
------------------------------------------------

-- | The basic loop for a 'WorkerThread': get the socket from the server mainloop,
-- receive a request, handle it, then put myself back into the 'WorkerPool'.
workerLoop :: Maybe (MVar WorkerPool) ->
              Environment ->
              Maybe (Chan Socket) ->
              IO ()
workerLoop Nothing e Nothing
         = do workerProcessRequest Nothing e
workerLoop (Just workerPoolMVar) e (Just chan)
         = do sock      <- readChan chan
              workerProcessRequest (Just sock) e
              putWorkerThread workerPoolMVar chan
              workerLoop (Just workerPoolMVar) e (Just chan)
workerLoop _ e _ = error "Turbinado.Server: workerLoop: Not CGI mode and not Server mode."

-- | Basic request handling: setup the 'Environment' for this request,
-- run the real requestHandler, then ship the response back to the client.
workerProcessRequest :: Maybe Socket -> Environment -> IO ()
workerProcessRequest msock e
    = (do mytid <- myThreadId
          e' <- runController (sequence_ [ addEmptyResponse
                                         , addViewDataToEnvironment
                                         , addSettingsToEnvironment
                                         , receiveRequest msock
                                         , tryStaticContent 
                                         ]) e
          case (isResponseComplete e') of
            True  -> sendResponse msock e'
            False -> do e'' <- runController processRequest e'
                        sendResponse msock e''
      ) 
        `catchTurbinado` (\ex -> handleTurbinado msock ex e)
        `catch` (\ex -> handleError msock ex e)
        `finally` (when (isJust msock) 
                     (sClose $ fromJust msock)
                  )



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
            tid <- forkIO $ workerLoop (Just mv) e' (Just chan)
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


