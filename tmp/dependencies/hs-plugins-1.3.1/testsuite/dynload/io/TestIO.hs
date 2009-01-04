{-# OPTIONS -fglasgow-exts -cpp #-}
--
-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- LGPL version 2.1 or later (see http://www.gnu.org/copyleft/lesser.html)
--

module TestIO ( resource_dyn )  where

import API
import AltData.Dynamic

import System.IO
import System.Posix.Types   ( ProcessID, Fd )
import System.Posix.Process ( forkProcess, executeFile, getProcessID )
import System.Posix.IO      ( createPipe, stdInput, 
                              stdOutput, fdToHandle, closeFd, dupTo )

resource_dyn :: Dynamic
resource_dyn = toDyn resource

resource :: TestIO
resource = testio { field = date }


--
-- call a shell command , returning it's output
--
date :: IO String
date = do (hdl,_,_) <- catch (popen "/bin/date") (\_->error "popen failed")
          hGetLine hdl 

------------------------------------------------------------------------
--
-- my implementation of $val = `cmd`; (if this was perl)
--
-- provide similar functionality to popen(3), 
-- along with bidirectional ipc via pipes
-- return's the pid of the child process
--
-- there are two different forkProcess functions. the pre-620 was a
-- unix-fork style function, and the modern function has semantics more
-- like the Awkward-Squad paper. We provide implementations of popen
-- using both versions, depending on which GHC the user wants to try.
-- 

popen :: FilePath -> IO (Handle, Handle, ProcessID)
popen cmd = do
        (pr, pw) <- createPipe
        (cr, cw) <- createPipe    

        -- parent --
        let parent = do closeFd cw
                        closeFd pr
        -- child --
        let child  = do closeFd pw
                        closeFd cr 
                        exec cmd (pr,cw)
                        error "exec cmd failed!" -- typing only

-- if the parser front end understood cpp, this would work
-- #if __GLASGOW_HASKELL__ >= 601
        pid <- forkProcess child -- fork child
        parent                   -- and run parent code
-- #else
--          p   <- forkProcess
--          pid <- case p of
--                  Just pid -> parent >> return pid
--                  Nothing  -> child
-- #endif

        hcr <- fdToHandle cr
        hpw <- fdToHandle pw

        return (hcr,hpw,pid)

--
-- execve cmd in the child process, dup'ing the file descriptors passed
-- as arguments to become the child's stdin and stdout.
--
exec :: FilePath -> (Fd,Fd) -> IO ()
exec cmd (pr,cw) = do
        dupTo pr stdInput
        dupTo cw stdOutput
        executeFile cmd False [] Nothing

------------------------------------------------------------------------
