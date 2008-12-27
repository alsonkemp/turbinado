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
import Turbinado.Server.Handlers.ErrorHandler (handleError, handleTurbinado)
import Turbinado.Server.Handlers.RequestHandler (requestHandler)
import Turbinado.Server.Handlers.SessionHandler
import Turbinado.Server.Network (receiveRequest, sendResponse)
import Turbinado.Server.StandardResponse (pageResponse)
import Turbinado.Server.StaticContent

data Flag 
     = Port Integer
     | Eval String 
     | Help
    deriving (Show, Eq)

options :: [OptDescr Flag]
options =
    [ Option ['p']     ["port"] (ReqArg (Port . read) "PORTNUMBER") "start hsp runtime on port PORTNUMBER"
    , Option ['e']     ["eval"] (ReqArg Eval "FILE")                "eval page with hsp runtime"
    , Option ['h','?'] ["help"] (NoArg Help)                        "show this message"
    ]

main =
    do args <- getArgs
       case getOpt Permute options args of
          ([Port n],[],[]) -> startServer (fromIntegral n)
          (opts,[],[]) | Help `elem` opts -> putStr $ usageInfo header options
          (_,_,errs)       -> ioError (userError (concat errs ++ usageInfo header options))
    where
     header = "Usage: turbinado [OPTION]"


startServer :: PortNumber -> IO ()
startServer pnr
    = withSocketsDo $ 
      do e <- runController 
               (sequence_ $ [ addLoggerToEnvironment
                            , addCodeStoreToEnvironment
                            , addMimeTypesToEnvironment "Config/mime.types"]
                            ++ customSetupFilters
               ) 
               newEnvironment
         sock <- listenOn $ PortNumber pnr
         workerPoolMVar <- newMVar $ WorkerPool 0 [] []
         mainLoop sock workerPoolMVar e
    where
      --mainLoop :: Socket -> WorkerPool -> IO ()
      mainLoop sock workerPoolMVar e = 
          do (sock', sockAddr) <- accept sock
             WorkerThread _ chan <- getWorkerThread workerPoolMVar e
             writeChan chan sock'
             mainLoop sock workerPoolMVar e


------------------------------------------------
-- | Worker stuff
------------------------------------------------

workerLoop :: MVar WorkerPool ->
              Environment ->
              Chan Socket      ->
              IO ()
workerLoop workerPoolMVar e chan
    = do mainLoop
    where
      mainLoop
          = do sock      <- readChan chan
               handleRequest sock e
               putWorkerThread workerPoolMVar chan
               mainLoop

handleRequest :: Socket -> Environment -> IO ()
handleRequest sock e
    = (do mytid <- myThreadId
          e' <- runController (sequence_ [ addViewDataToEnvironment
                                         , addSettingsToEnvironment
                                         , receiveRequest sock
                                         , tryStaticContent 
                                         , addRoutesToEnvironment ]) e
          case (isResponseComplete e') of
            True  -> sendResponse sock e'
            False -> do e'' <- runController requestHandler e'
                        sendResponse sock e''
      ) 
        `catchTurbinado` (\ex -> handleTurbinado sock ex e)
        `catch` (\ex -> handleError sock ex e)
        `finally` (sClose sock)



------------------------------------------------
-- | Worker Pool stuff
------------------------------------------------

type ExpiresTime = UTCTime
data WorkerThread = WorkerThread ThreadId (Chan Socket)
data WorkerPool = WorkerPool { numWorkers :: Int,
                               idleWorkers :: [WorkerThread],
                               busyWorkers :: [(WorkerThread, ExpiresTime)]}

-- TODO: add a Maximum # of threads
--getWorkerThread :: MVar WorkerPool -> IO WorkerThread
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

putWorkerThread mv chan = do
               WorkerPool n is bs <- takeMVar mv
               mytid <- myThreadId
               let bs' = filter (\(WorkerThread tid _, _) -> tid /= mytid) bs
               putMVar mv $ WorkerPool n ((WorkerThread mytid chan):is) bs'



timeout :: Int -> ThreadId -> IO ()
timeout time thid
    = do threadDelay time
         throwTurbinadoTo thid TimedOut

-- conf. files? Indeed!
stdTimeOut :: Integer
stdTimeOut = 90


