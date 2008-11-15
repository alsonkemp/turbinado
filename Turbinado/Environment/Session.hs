module Turbinado.Environment.Session (
	  Session	-- ^ Abstract
	-- * Functions used in HSP
	, getVarValue	-- ^ :: Session -> Key -> (Maybe Value)
	, setVarValue	-- ^ :: Session -> Key -> Value -> ()
	, deleteVar	-- ^ :: Session -> Key -> ()
	, abandon	-- ^ :: Session -> ()
	, setExpires	-- ^ :: Session -> UTCTime -> ()
	-- * Functions used by the RTS
	, isSession	-- ^ :: Session -> Bool
	, getSessionId  -- ^ :: Session -> (Maybe SessionId)
	, getExpires	-- ^ :: Session -> Expires
	, initSession	-- ^ :: [(Key, Value)] -> Session
	, noSession	-- ^ :: Session
	, getNewVars	-- ^ :: Session -> [(Key, (Value, Expires))]
	, getUpdatedVars -- ^ :: Session -> [(Key, (Value, Expires))]
	, getDeletedVars -- ^ :: Session -> [Key]
	) where

import qualified Data.Map as M
import Data.Time

-------------------------------------
-- Help types

type Expires = Maybe UTCTime
type Key = String
type Value = String
type SessionId = Int

neverExpire :: Expires
neverExpire = Nothing

expire :: UTCTime -> Expires
expire = Just

data Status = New | Orig | Updated | Deleted
	deriving (Eq)

updateStatus :: Status -> Status
updateStatus s = case s of
	New -> New
	_   -> Updated

----------------------------------------
-- The main datatypes

-- | The 'Session' datatype is basically a data repository.
-- To keep tracks of updates, we use an extra repository.
newtype Session = Session (Maybe SessionData)

data SessionData = SessionData {
	sessionId :: Maybe SessionId, 
	expires :: Expires,
	dataRep :: M.Map Key (Value,Expires,Status)
	}

-- | Create a new 'Session' object with initial data.
initSession :: SessionId -> Expires -> [(Key, (Value, Expires))] -> Session
initSession sid exps initData = 
	let dat = map (\(k,(v,e)) -> (k,(v,e,Orig))) initData 
            rep = M.fromList dat
	    sd  = SessionData { 
			dataRep = rep,
			expires = exps, 
			sessionId = Just sid } in
	 Session (Just sd)

noSession :: Session
noSession = Session Nothing

---------------------------------------
-- Operate on sessions

-- | Retrieve the value of a variable in the repository.
getVarValue :: Session -> Key -> Maybe Value
getVarValue (Session Nothing) k = Nothing
getVarValue (Session (Just sd)) k = 
	case (M.lookup k (dataRep sd)) of
		Nothing -> Nothing
		Just (v,e,Deleted) -> Nothing
		Just (v,e,_) -> Just v

setVarValue :: Session -> Key -> Value -> Session
setVarValue (Session Nothing) k v = error "Tried to setVarValue without a valid session"
setVarValue (Session (Just sd)) k v =
	 case (M.lookup k (dataRep sd)) of
		Nothing -> Session $ Just $ sd {dataRep = M.insert k (v, neverExpire, New) (dataRep sd)}
		Just (_,e,st) -> Session $ Just $ sd {dataRep = M.insert k (v, e, updateStatus st) (dataRep sd)}
		
deleteVar :: Session -> Key -> Session
deleteVar (Session Nothing) k = (Session Nothing)
deleteVar (Session (Just sd)) k = Session $ Just $ sd {dataRep = M.delete k (dataRep sd)}

abandon :: Session -> Session
abandon (Session mvs) = (Session Nothing)

setExpires :: Session -> UTCTime -> Session
setExpires (Session Nothing) ct = error "Tried to setVarValue without a valid session"
setExpires (Session (Just sd)) ct = Session $ Just $ sd {expires = expire ct} 

-----------------------------------------
-- Used by HSPR

isSession :: Session -> Bool
isSession (Session Nothing) = False
isSession _         = True


getSessionId :: Session -> Maybe SessionId
getSessionId (Session Nothing) = Nothing
getSessionId (Session (Just sd)) = sessionId sd

getExpires :: Session -> Expires
getExpires (Session Nothing) = Nothing
getExpires (Session (Just sd)) = expires sd


getVars :: Status -> Session -> [(Key, (Value, Expires))]
getVars status (Session Nothing) = []
getVars status (Session (Just sd)) = 
  let vals    = M.toList (dataRep sd)
      newVals = filter (\(_,(_,_,st)) -> st == status) vals
  in map (\(k,(v,e,_)) -> (k,(v,e))) newVals

getNewVars, getUpdatedVars :: Session -> [(Key, (Value, Expires))]
getNewVars = getVars New
getUpdatedVars = getVars Updated

getDeletedVars :: Session -> [Key]
getDeletedVars (Session Nothing) = []
getDeletedVars s = map fst $ getVars Deleted s
