{- arch-tag: Growl log handlers
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

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
   Module     : System.Log.Handler.Growl
   Copyright  : Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>
   License    : GNU LGPL, version 2.1 or above

   Maintainer : Richard M. Neswold, Jr. <rich.neswold@gmail.com>
   Stability  : provisional
   Portability: portable

Simple log handlers

Written by Richard M. Neswold, Jr. rich.neswold\@gmail.com
-}

module System.Log.Handler.Growl(addTarget, growlHandler)
    where

import Data.Char
import Data.Word
import Network.Socket
import Network.BSD
import System.Log
import System.Log.Handler

data GrowlHandler = GrowlHandler { priority :: Priority,
                                   appName :: String,
                                   skt :: Socket,
                                   targets :: [HostAddress] }

instance LogHandler GrowlHandler where

    setLevel gh p = gh { priority = p }

    getLevel = priority

    emit gh lr _ = let pkt = buildNotification gh nmGeneralMsg lr
		   in mapM_ (sendNote (skt gh) pkt) (targets gh)

    close gh = let pkt = buildNotification gh nmClosingMsg
			 (WARNING, "Connection closing.")
                   s   = skt gh
	       in mapM_ (sendNote s pkt) (targets gh) >> sClose s

sendNote :: Socket -> String -> HostAddress -> IO Int
sendNote s pkt ha = sendTo s pkt (SockAddrInet 9887 ha)

-- Right now there are two "notification names": "message" and
-- "disconnecting". All log messages are sent using the "message"
-- name. When the handler gets closed properly, the "disconnecting"
-- notification gets sent.

nmGeneralMsg :: String
nmGeneralMsg = "message"

nmClosingMsg :: String
nmClosingMsg = "disconnecting"

{- | Creates a Growl handler. Once a Growl handler has been created,
     machines that are to receive the message have to be specified. -}

growlHandler :: String          -- ^ The name of the service
             -> Priority        -- ^ Priority of handler
             -> IO GrowlHandler
growlHandler nm pri =
    do { s <- socket AF_INET Datagram 0
       ; return GrowlHandler { priority = pri, appName = nm,
                               skt = s, targets = [] }
       }

-- Converts a Word16 into a string of two characters. The value is
-- emitted in network byte order.

emit16 :: Word16 -> String
emit16 v = let (h, l) = (fromEnum v) `divMod` 256 in [chr h, chr l]

emitLen16 :: [a] -> String
emitLen16 = emit16 . fromIntegral . length

-- Takes a Service record and generates a network packet
-- representing the service.

buildRegistration :: GrowlHandler -> String
buildRegistration s = concat fields
    where fields = [ ['\x1', '\x4'],
                     emitLen16 (appName s),
                     emitLen8 appNotes,
                     emitLen8 appNotes,
                     appName s,
                     foldl packIt [] appNotes,
                     ['\x0' .. (chr (length appNotes - 1))] ]
          packIt a b = a ++ (emitLen16 b) ++ b
	  appNotes = [ nmGeneralMsg, nmClosingMsg ]
          emitLen8 v = [chr $ length v]

{- | Adds a remote machine's address to the list of targets that will
     receive log messages. Calling this function sends a registration
     packet to the machine. This function will throw an exception if
     the host name cannot be found. -}

addTarget :: HostName -> GrowlHandler -> IO GrowlHandler
addTarget hn gh = do { he <- getHostByName hn
                     ; let ha = hostAddress he
                           sa = SockAddrInet 9887 ha
                       in do { sendTo (skt gh) (buildRegistration gh) sa
                             ; return gh { targets = ha:(targets gh) } } }

-- Converts a Priority type into the subset of integers needed in the
-- network packet's flag field.

toFlags :: Priority -> Word16
toFlags DEBUG = 12
toFlags INFO = 10
toFlags NOTICE = 0
toFlags WARNING = 2
toFlags ERROR = 3       -- Same as WARNING, but "sticky" bit set
toFlags CRITICAL = 3    -- Same as WARNING, but "sticky" bit set
toFlags ALERT = 4
toFlags EMERGENCY = 5   -- Same as ALERT, but "sticky" bit set

-- Creates a network packet containing a notification record.

buildNotification :: GrowlHandler
		  -> String
                  -> LogRecord
		  -> String
buildNotification gh nm (p, msg) = concat fields
    where fields = [ ['\x1', '\x5'],
                     emit16 (toFlags p),
                     emitLen16 nm,
                     emit16 0,
                     emitLen16 msg,
                     emitLen16 (appName gh),
                     nm,
                     [],
                     msg,
                     appName gh ]
