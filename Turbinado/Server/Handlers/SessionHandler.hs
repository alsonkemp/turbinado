module Turbinado.Server.Handlers.SessionHandler (
        SessionHandler(..),
        SessionId, Key, Value, Expires, SessionData, SessionItem
        ) where

import Data.Time

type SessionId = Int
type Key = String
type Value = String
type Expires = Maybe UTCTime

type SessionData = (Expires, [SessionItem])
type SessionItem = (Key, (Value, Expires))

----------------------------------------------------
-- The SessionHandler class

class SessionHandler sh where
 lookupData    :: sh -> SessionId -> IO (Maybe SessionData)
 insertNew     :: sh -> SessionId -> Expires -> IO ()
 insertNewData :: sh -> SessionId -> SessionItem -> IO ()
 updateExpires :: sh -> SessionId -> Expires -> IO ()
 updateData    :: sh -> SessionId -> SessionItem -> IO ()
 deleteSession :: sh -> SessionId -> IO ()
 deleteData    :: sh -> SessionId -> Key -> IO ()
