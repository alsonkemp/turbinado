{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Turbinado.SessionHandlers.GenericDB
-- Copyright   :  (c) Niklas Broberg 2006
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Niklas Broberg, d00nibro@dtek.chalmers.se
-- Stability   :  experimental
-- Portability :  requires HaskellDB
--
-- A generic SessionHandler template for use with databases that are
-- supported by HaskellDB. To use it, write a session handler that imports
-- this module and provides the proper connect function. See also the DbSpec.hs
-- file to see how to set up the database proper.
-----------------------------------------------------------------------------
module Turbinado.SessionHandlers.GenericDB (
        mkSessionDB                               
        ) where

import Database.HaskellDB
import Database.HaskellDB.HDBRec (RecCons, RecNil)

import Turbinado.SessionHandler (SessionId, Key, Value, Expires, SessionData, SessionItem)
import qualified Turbinado.SessionHandler as SH

import Turbinado.SessionHandlers.GenericDB.SessionDBInfo
import Turbinado.SessionHandlers.GenericDB.SessionDBInfo.Sessions (sessions, sessionId, expires)
import Turbinado.SessionHandlers.GenericDB.SessionDBInfo.SessionData (sessionData, datakey, value, dataExpires)

import qualified Turbinado.SessionHandlers.GenericDB.SessionDBInfo.Sessions as S (SessionId, Expires)
import qualified Turbinado.SessionHandlers.GenericDB.SessionDBInfo.SessionData as SD (sessionId, SessionId, DataKey, Value, DataExpires)

--import Turbinado.PrintDebug
debugM e _ = return ()


data SessionDB = SessionDB { 
        dbconnect :: forall a . (Database -> IO a) -> IO a
        }

instance SH.SessionHandler SessionDB where
 lookupData    = lookupData
 insertNew     = insertNew
 insertNewData = insertNewData
 updateExpires = updateExpires
 updateData    = updateData
 deleteSession = deleteSession
 deleteData    = deleteData

----------------------------------------------------
-- Creating a SessionDB object

mkSessionDB :: (forall a . (Database -> IO a) -> IO a) -> SessionDB
mkSessionDB = SessionDB

----------------------------------------------------
-- Lookup of session data

lookupData :: SessionDB -> SessionId -> IO (Maybe SessionData)
lookupData sdb sid = do
        debugM e $ "Performing lookup in DB for " ++ show sid ++ " ... "
        recs <- (dbconnect sdb) 
        		(\db -> do debugM e . show $ selectSId sid
        			   query db $ selectSId sid)
        debugM e "Done!"
        case recs of
         [] -> return Nothing
         rs@(r:_) -> do let exps = r!expires
        	      	    ds = map getData rs
        		return $ Just (exps, ds)
  where getData r = let k = r!datakey
        		v = r!value
        		e = r!dataExpires
        	     in (k, (v,e))

selectSId :: SessionId -> Query (Rel (
        	RecCons S.SessionId	(Expr SessionId) (
        	RecCons S.Expires	(Expr Expires) 	 (
        	RecCons SD.DataKey	(Expr Key) 	 (
        	RecCons SD.Value	(Expr Value) 	 (
        	RecCons SD.DataExpires	(Expr Expires)
        	RecNil ))))))
selectSId sid = do
        ses <- table sessions
        dat <- table sessionData
        restrict (ses!sessionId .==. constant sid .&&. ses!sessionId .==. dat!SD.sessionId)
        project ( sessionId << ses!sessionId 
        	# expires << ses!expires
        	# datakey << dat!datakey
        	# value << dat!value
        	# dataExpires << dat!dataExpires
        	)

----------------------------------------------------
-- New session

insertNew :: SessionDB -> SessionId -> Expires -> IO ()
insertNew sdb sid exps = (dbconnect sdb) $ insertSession sid exps

insertSession :: SessionId -> Expires -> (Database -> IO ())
insertSession sid exps = \db -> insert db sessions 
        ( sessionId << constant sid 
        # expires   << constant exps )


insertNewData :: SessionDB -> SessionId -> SessionItem -> IO ()
insertNewData sdb sid dat = (dbconnect sdb) $ insertSessionData sid dat

insertSessionData :: SessionId -> SessionItem -> (Database -> IO ())
insertSessionData sid (k,(v,e)) = \db -> insert db sessionData
        ( SD.sessionId << constant sid
        # datakey      << constant k
        # value        << constant v
        # dataExpires  << constant e )

----------------------------------------------------
-- Update session 

updateExpires :: SessionDB -> SessionId -> Expires -> IO ()
updateExpires sdb sid exps = (dbconnect sdb) $ updateExp sid exps

updateExp :: SessionId -> Expires -> (Database -> IO ())
updateExp sid exps = \db -> update db sessions
        (\r -> r!sessionId .==. constant sid) 
        (\r -> expires << constant exps)

updateData :: SessionDB -> SessionId -> (Key, (Value, Expires)) -> IO ()
updateData sdb sid dat = (dbconnect sdb) $ updateSessionData sid dat

updateSessionData :: SessionId -> (Key, (Value, Expires)) -> (Database -> IO ())
updateSessionData sid (k,(v,e)) = \db -> update db sessionData
        (\r -> r!SD.sessionId .==. constant sid)
        (\r -> datakey << constant k # value << constant v # dataExpires << constant e)

----------------------------------------------------
-- Delete session 

deleteSession :: SessionDB -> SessionId -> IO ()
deleteSession sdb sid = (dbconnect sdb) $ deleteSess sid

deleteSess :: SessionId -> (Database -> IO ())
deleteSess sid = \db -> do 
        delete db sessionData 
        	(\r -> r!SD.sessionId .==. constant sid)
        delete db sessions
        	(\r -> r!sessionId .==. constant sid)

deleteData :: SessionDB -> SessionId -> Key -> IO ()
deleteData sdb sid k = (dbconnect sdb) $ deleteSessionData sid k

deleteSessionData :: SessionId -> Key -> (Database -> IO ())
deleteSessionData sid k = \db -> delete db sessionData
        	(\r -> r!SD.sessionId .==. constant sid 
        	    .&&. r!datakey .==. constant k)



-- main = lookupData (SessionDB psqlconnect) 1 >>= print
