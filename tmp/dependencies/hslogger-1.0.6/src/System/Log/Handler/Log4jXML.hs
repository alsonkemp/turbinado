{- arch-tag: log4j XMLLayout log handler
Copyright (C) 2007-2008 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-}

{- |
   Module     : System.Log.Handler.Log4jXML
   Copyright  : Copyright (C) 2007-2008 John Goerzen
   License    : GNU LGPL, version 2.1 or above

   Maintainer : bjorn.buckwalter@gmail.com
   Stability  : experimental
   Portability: GHC only?

log4j[1] XMLLayout log handlers.

Written by Bjorn Buckwalter, bjorn.buckwalter\@gmail.com
-}


module System.Log.Handler.Log4jXML ( 
    
    -- * Introduction

    {- | This module provides handlers for hslogger that are
    compatible with log4j's XMLLayout. In particular log messages
    created by the handlers can be published directly to the GUI-based
    log viewer Chainsaw v2[2].

    The set of log levels in hslogger is richer than the basic set
    of log4j levels. Two sets of handlers are provided with hslogger4j,
    one which produces logs with hslogger's levels and one which
    \"demotes\" them to the basic log4j levels. If full hslogger
    levels are used some Java installation (see below) is necessary
    to make Chainsaw aware of them.

    Usage of the handlers in hslogger4j is analoguous to usage of
    the 'System.Log.Handler.Simple.StreamHandler' and
    'System.Log.Handler.Simple.FileHandler' in "System.Log.Handler.Simple".
    The following handlers are provided: -}

    -- ** Handlers with hslogger levels 
    log4jStreamHandler,
    log4jFileHandler,
    
    -- ** Handlers with log4j levels
    log4jStreamHandler',
    log4jFileHandler'


    -- * Java install process

    {- | This is only necessary if you want to use the hslogger levels.

    Add @hslogger4j.jar@ from @contrib\/java@ to your classpath.
    To use you will also need to have the jars @log4j-1.3alpha-7.jar@
    and @log4j-xml-1.3alpha-7.jar@ that are distributed with Chainsaw
    on your classpath.

    (On Mac OS X I added all three jars to @~\/Library\/Java\/Extensions@.
    It seems that it is not sufficient that Chainsaw already includes
    its jars in the classpath when launching - perhaps the plugin
    classloader does not inherit Chainsaw's classpath. Adding the
    jars to @~\/.chainsaw\/plugins@ wouldn't work either.)

    If for whatever reason you have to rebuild the hslogger4j jar
    just run @ant@[3] in the @contrib\/java@ directory. The new jar
    will be created in the @contrib\/java\/dist@ directory. The Java
    source code is copyright The Apache Software Foundation and
    licensed under the Apache Licence version 2.0. -}


    -- * Chainsaw setup

    {- | If you are only using the basic log4j levels just use
    Chainsaw's regular facilities to browse logs or listen for log
    messages (e.g. @XMLSocketReceiver@).

    If you want to use the hslogger levels the easiest way to set
    up Chainsaw is to load the plugins in @hslogger4j-plugins.xml@
    in @contrib\/java@ when launching Chainsaw. Two receivers will
    be defined, one that listens for logmessages and one for reading
    log files.  Edit the properties of those receivers as needed
    (e.g. @port@, @fileURL@) and restart them. You will also want
    to modify Chainsaw's formatting preferences to display levels
    as text instead of icons. -}


    -- * Example usage

    {- | In the IO monad:

    > lh2 <- log4jFileHandler "log.xml" DEBUG
    > updateGlobalLogger rootLoggerName (addHandler lh2)

    > h  <- connectTo "localhost" (PortNumber 4448)
    > lh <- log4jStreamHandler h NOTICE
    > updateGlobalLogger rootLoggerName (addHandler lh)
    -}

    -- * References

    {- |
    (1) <http://logging.apache.org/log4j/>

    (2) <http://logging.apache.org/chainsaw/>

    (3) <http://ant.apache.org/>
    -}

    ) where

import Control.Concurrent (ThreadId, myThreadId)  -- myThreadId is GHC only!
import Control.Concurrent.MVar
import Data.List (isPrefixOf)
import System.IO
import System.Locale (defaultTimeLocale)
import Data.Time
import System.Log
import System.Log.Handler.Simple (GenericHandler (..))


-- Handler that logs to a handle rendering message priorities according
-- to the supplied function.
log4jHandler :: (Priority -> String) -> Handle -> Priority -> IO (GenericHandler Handle)
log4jHandler showPrio h pri = do
    lock <- newMVar ()
    let mywritefunc hdl (prio, msg) loggername = withMVar lock (\_ -> do
        time   <- getCurrentTime
        thread <- myThreadId
        hPutStrLn hdl (show $ createMessage loggername time prio thread msg)
        hFlush hdl
        )
    return (GenericHandler { priority  = pri,
                             privData  = h,
                             writeFunc = mywritefunc,
                             closeFunc = \x -> return () })
    where
        -- Creates an XML element representing a log4j event/message.
        createMessage :: String -> UTCTime -> Priority -> ThreadId -> String -> XML
        createMessage logger time prio thread msg = Elem "log4j:event"
            [ ("logger"   , logger       )
            , ("timestamp", millis time  )
            , ("level"    , showPrio prio)
            , ("thread"   , show thread  )
            ]
            (Just $ Elem "log4j:message" [] (Just $ CDATA msg))
            where
                -- This is an ugly hack to get a unix epoch with milliseconds.
                -- The use of "take 3" causes the milliseconds to always be
                -- rounded downwards, which I suppose may be the expected
                -- behaviour for time.
                millis t = formatTime defaultTimeLocale "%s" t
                    ++ (take 3 $ formatTime defaultTimeLocale "%q" t)


-- | Create a stream log handler that uses hslogger priorities.
log4jStreamHandler :: Handle -> Priority -> IO (GenericHandler Handle)
log4jStreamHandler = log4jHandler show

{- | Create a stream log handler that uses log4j levels (priorities). The
   priorities of messages are shoehorned into log4j levels as follows:

@
    DEBUG                  -> DEBUG
    INFO, NOTICE           -> INFO
    WARNING                -> WARN
    ERROR, CRITICAL, ALERT -> ERROR
    EMERGENCY              -> FATAL
@

   This is useful when the log will only be consumed by log4j tools and
   you don't want to go out of your way transforming the log or configuring
   the tools. -}
log4jStreamHandler' :: Handle -> Priority -> IO (GenericHandler Handle)
log4jStreamHandler' = log4jHandler show' where
    show' :: Priority -> String
    show' NOTICE    = "INFO"
    show' WARNING   = "WARN"
    show' CRITICAL  = "ERROR"
    show' ALERT     = "ERROR"
    show' EMERGENCY = "FATAL"
    show' p         = show p  -- Identical for DEBUG, INFO, ERROR.


-- | Create a file log handler that uses hslogger priorities.
log4jFileHandler :: FilePath -> Priority -> IO (GenericHandler Handle)
log4jFileHandler fp pri = do
                          h <- openFile fp AppendMode
                          sh <- log4jStreamHandler h pri
                          return (sh{closeFunc = hClose})

{- | Create a file log handler that uses log4j levels (see
   'log4jStreamHandler'' for mappings). -}
log4jFileHandler' :: FilePath -> Priority -> IO (GenericHandler Handle)
log4jFileHandler' fp pri = do
                           h <- openFile fp AppendMode
                           sh <- log4jStreamHandler' h pri
                           return (sh{closeFunc = hClose})


-- A type for building and showing XML elements. Could use a fancy XML
-- library but am reluctant to introduce dependencies.
data XML = Elem  String [(String, String)] (Maybe XML)
         | CDATA String

instance Show XML where
    show (CDATA s) = "<![CDATA[" ++ escapeCDATA s ++ "]]>" where
        escapeCDATA = replace "]]>" "]]&lt;"  -- The best we can do, I guess.
    show (Elem name attrs child) = "<" ++ name ++ showAttrs attrs ++ showChild child where
        showAttrs []         = ""
        showAttrs ((k,v):as) = " " ++ k ++ "=\"" ++ escapeAttr v ++ "\""
                             ++ showAttrs as
            where escapeAttr = replace "\"" "&quot;"
                             . replace "<" "&lt;"
                             . replace "&" "&amp;"
        showChild Nothing  = "/>"
        showChild (Just c) = ">" ++ show c ++ "</" ++ name ++ ">"


-- Replaces instances of first list by second list in third list.
-- Definition blatantly stoled from jethr0's comment at
-- http://bluebones.net/2007/01/replace-in-haskell/. Can be swapped
-- with definition (or import) from MissingH.
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _    _  [       ] = []
replace from to xs@(a:as) = if isPrefixOf from xs
    then to ++ drop (length from) xs else a : replace from to as

