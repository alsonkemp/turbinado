-----------------------------------------------------------------------------
-- |
-- Module      :  Turbinado.Environment.Session.CookieSession
-- Copyright   :  (c) Niklas Broberg 2004, Michael Snoyman 2008-2009, Alson Kemp 2009
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  Alson Kemp, alson@alsonkemp.com
-- Stability   :  experimental
-- Portability :  requires undecidable and overlapping instances
--
-- Much of this code is lifted/derived from Niklas' HSP and from Michael's HWeb.
-----------------------------------------------------------------------------
module Turbinado.Environment.Session.CookieSession where

import Control.Monad.Trans
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Time
import System.IO

import qualified Data.Digest.MD5 as MD5
import Data.LargeWord (Word128)
import Data.Word (Word8)
import Codec.Encryption.Modes
import qualified Codec.Encryption.AES as AES
import qualified Codec.Binary.Base64 as Base64
import Codec.Utils
import qualified Network.HTTP.Headers as Headers
import Turbinado.Environment.Cookie
import Turbinado.Environment.Types
import Turbinado.Utility.Data

type Key = String
type Value = String


instance (HasEnvironment m) => HasSession m where
  newSession opts = let n = maybe
                              (error "'cookie-name' didn't exist in options passed to newSession")
                              id
                              (lookup "cookie-name" opts)
                    in _setSession $ emptySession { sessionName = Just n }
  hasValidSession = do e <- getEnvironment
                       case getSession e of
                        Nothing -> return False
                        Just s -> case expires s of
                                    Nothing -> return True
                                    Just t  -> do now <- liftIO $ getCurrentTime
                                                  return $ t > now
  retrieveSession opts = do let c = maybe
                                     (error "'cipher-key' didn't exist in options passed to retrieveSession")
                                     id
                                     (lookup "cipher-key" opts)
                                n = maybe
                                     (error "'cookie-name' didn't exist in options passed to retrieveSession")
                                     id
                                     (lookup "cookie-name" opts)
                            message'' <- getCookie n
                            e <- getEnvironment
                            case message'' of
                              Nothing  -> newSession opts
                              Just m'' -> let message' = maybeRead m'' in
                                          case message' of
                                            Nothing     -> newSession opts
                                            Just (m, h) -> do let messageBlocks = unCbc AES.decrypt 0 (w8ToKey $ MD5.hash $ stringToW8 c) (w8ToBlocks $ fromJust' "CookieSession : retrieveSession" $ Base64.decode m)
                                                                  hashCode = fromJust' "CookieSession : retreiveSession(2)" $ Base64.decode h
                                                                  hashCheck = MD5.hash $ blocksToW8 messageBlocks
                                                              if (hashCode == hashCheck)
                                                                then let s = read (w8ToString $ blocksToW8 messageBlocks) in
                                                                     case (expires s) of
                                                                       Nothing -> do _setSession s
                                                                       Just t -> do t' <- liftIO $ getCurrentTime
                                                                                    if (t > t')
                                                                                     then _setSession s
                                                                                     else newSession opts
                                                                else newSession opts
  persistSession opts  = do e <- getEnvironment
                            let s' = getSession e
                            case s' of
                              Nothing -> return ()
                              Just s -> do let c = maybe
                                                    (error "'cipher-key' didn't exist in options passed to persistSession")
                                                    id
                                                    (lookup "cipher-key" opts)
                                               ex = maybe
                                                      Nothing
                                                      maybeReadUTC
                                                      (lookup "session-expires" opts)
                                               message = stringToW8 $ show s
                                               cipheredMessage = Base64.encode $ blocksToW8 $ cbc AES.encrypt 0 (w8ToKey $ MD5.hash $ stringToW8 c) (w8ToBlocks message)
                                               hashCode = Base64.encode $ MD5.hash message
                                           setCookie 
                                             (Cookie {cookieName = fromJust' "CookieSession : persistSession" $ sessionName s
                                                     ,cookieValue = (show $ (cipheredMessage, hashCode))
                                                     ,cookieExpires = ex
                                                     ,cookieDomain = Nothing
                                                     ,cookiePath = Nothing
                                                     }
                                             )
  abandonSession = do e <- getEnvironment
                      let s = getSession e
                      setEnvironment $ e {getSession = Nothing}
                      case s of
                        Nothing -> return ()
                        Just s' -> deleteCookie (fromJust' "CookieSession : abandonSession" $ sessionName s')
  getSessionValue k = do s <- _getSession
                         return $ M.lookup k $ dataRep s
  setSessionValue k v = do s <- _getSession
                           let s' = s {dataRep = M.insert k v (dataRep s)}
                           _setSession s'
  deleteSessionKey k = do s <- _getSession
                          let s' = s {dataRep = M.delete k (dataRep s)}
                          _setSession s'
  getSessionExpires = (return . expires) =<< _getSession
  setSessionExpires ct = do s <- _getSession
                            let s' = s {expires = ct}
                            _setSession s'
  setSessionId sid = do s <- _getSession
                        let s' = s {sessionId = sid}
                        _setSession s'
  getSessionId  = (return . sessionId) =<< _getSession


--
-- * Helpers 
--
stringToW8 :: String -> [Word8]
stringToW8 = map (fromInteger . toInteger . fromEnum)

w128ToW8 :: Word128 -> [Word8]
w128ToW8 w128 = toOctets 256 w128

w8ToString :: [Word8] -> String
w8ToString = map (toEnum . fromInteger . toInteger)

blocksToString :: [[Word8]] -> String
blocksToString ws = concat $ map w8ToString ws

blocksToW8 :: [Word128] -> [Word8]
blocksToW8 ws = concat $ map w128ToW8 ws

w8ToKey :: [Word8] ->  Word128
w8ToKey ws = fromInteger $ foldl (\acc i -> acc*256 + toInteger i) (0::Integer) ws

w8ToBlocks :: [Word8] -> [Word128]
w8ToBlocks ws = map w8ToKey $ breakup ws
  where breakup [] = []
        breakup ws = (take 16 ws) : (breakup $ drop 16 ws)

maybeRead = listToMaybe . map fst . filter (null . snd) . reads
maybeReadUTC :: String -> Maybe UTCTime
maybeReadUTC = listToMaybe . map fst . filter (null . snd) . reads

_getSession   :: (HasEnvironment m) => m Session
_getSession   = getEnvironment >>= (return . fromJust' "CookieSession : _getSession" . getSession)

_setSession   :: (HasEnvironment m) => Session -> m ()
_setSession s = getEnvironment >>= (\e -> setEnvironment $ e {getSession = Just s})


