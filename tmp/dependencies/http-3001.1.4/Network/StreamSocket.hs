-----------------------------------------------------------------------------
-- |
-- Module      :  Network.StreamSocket
-- Copyright   :  (c) Warrick Gray 2002, Bjorn Bringert 2003-2004, Simon Foster 2004, 2007 Robin Bate Boerop.
-- License     :  BSD
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- Socket Stream instance. Originally part of Gray's\/Bringert's HTTP module.
--
-- * Changes by Robin Bate Boerop <robin@bateboerop.name>:
--      - Made dependencies explicit in import statements.
--      - Removed false dependencies in import statements.
--      - Created separate module for instance Stream Socket.
--
-- * Changes by Simon Foster:
--      - Split module up into to sepearate Network.[Stream,TCP,HTTP] modules
--      
-----------------------------------------------------------------------------
module Network.StreamSocket
   ( handleSocketError
   , myrecv
   ) where

import Network.Stream
   ( Stream(..), ConnError(ErrorReset, ErrorMisc), Result
   )
import Network.Socket
   ( Socket, getSocketOption, shutdown, send, recv, sClose
   , ShutdownCmd(ShutdownBoth), SocketOption(SoError)
   )

import Control.Monad (liftM)
import Control.Exception as Exception (Exception, catch, throw)
import System.IO.Error (catch, isEOFError)

-- | Exception handler for socket operations.
handleSocketError :: Socket -> Exception -> IO (Result a)
handleSocketError sk e =
    do se <- getSocketOption sk SoError
       case se of
          0     -> throw e
          10054 -> return $ Left ErrorReset  -- reset
          _     -> return $ Left $ ErrorMisc $ show se

instance Stream Socket where
    readBlock sk n = (liftM Right $ fn n) `Exception.catch` (handleSocketError sk)
        where
            fn x = do { str <- myrecv sk x
                      ; let len = length str
                      ; if len < x
                          then ( fn (x-len) >>= \more -> return (str++more) )                        
                          else return str
                      }

    -- Use of the following function is discouraged.
    -- The function reads in one character at a time, 
    -- which causes many calls to the kernel recv()
    -- hence causes many context switches.
    readLine sk = (liftM Right $ fn "") `Exception.catch` (handleSocketError sk)
            where
                fn str =
                    do { c <- myrecv sk 1 -- like eating through a straw.
                       ; if null c || c == "\n"
                           then return (reverse str++c)
                           else fn (head c:str)
                       }
    
    writeBlock sk str = (liftM Right $ fn str) `Exception.catch` (handleSocketError sk)
        where
            fn [] = return ()
            fn x  = send sk x >>= \i -> fn (drop i x)

    -- This slams closed the connection (which is considered rude for TCP\/IP)
    close sk = shutdown sk ShutdownBoth >> sClose sk

myrecv :: Socket -> Int -> IO String
myrecv _ 0 = return ""
myrecv sock len =
    let handler e = if isEOFError e then return [] else ioError e
        in System.IO.Error.catch (recv sock len) handler

