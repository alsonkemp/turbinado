module Turbinado.Server.Handlers.SessionHandlers.Simple (getSessionHandler) where

import Turbinado.Server.Handlers.SessionHandler

import Control.Concurrent.MVar

import System.IO
import System.Directory

data SimpleSessionHandler = 
        SimpleSH {
        	dirpath :: FilePath,
        	lock :: MVar ()
        	}

getSessionHandler :: IO SimpleSessionHandler
getSessionHandler = do
        l <- newMVar ()
        return $ SimpleSH "tmp/sessions" l

sidPath :: SimpleSessionHandler -> SessionId -> FilePath
sidPath ssh sid = (dirpath ssh) ++ "/" ++ show sid


instance SessionHandler SimpleSessionHandler where
 lookupData         = lookupSid
 insertNew        = createNewSidFile
 insertNewData        = insertNewSessionData
 updateExpires        = updateExpiresLine
 updateData        = updateDataItem
 deleteSession        = deleteSidFile
 deleteData        = deleteDataItem


deleteSidFile :: SimpleSessionHandler -> SessionId -> IO ()
deleteSidFile ssh sid = removeFile $ sidPath ssh sid



createNewSidFile :: SimpleSessionHandler -> SessionId -> Expires -> IO ()
createNewSidFile ssh sid exp = do
        acquire ssh
        h <- openFile (sidPath ssh sid) WriteMode
        hPutStrLn h (show exp)
        hClose h
        release ssh

insertNewSessionData :: SimpleSessionHandler -> SessionId -> SessionItem -> IO ()
insertNewSessionData ssh sid dat = do
        acquire ssh
        appendFile (sidPath ssh sid) (show dat ++ "\n")
        release ssh

lookupSid :: SimpleSessionHandler -> SessionId -> IO (Maybe SessionData)
lookupSid ssh sid = do 
        acquire ssh
        let fp = sidPath ssh sid
        ex <- doesFileExist fp
        if ex
         then do h <- openFile fp ReadMode
        	 exp <- hGetLine h
        	 raw <- hGetContents h
        	 hClose h
        	 release ssh
        	 let dat  = map read $ lines raw
        	     expi = read exp
        	 return $ Just (expi, dat)
         else release ssh >> return Nothing


updateExpiresLine :: SimpleSessionHandler -> SessionId -> Expires -> IO ()
updateExpiresLine ssh sid exp = do
        acquire ssh
        let fp = sidPath ssh sid
        h <- openFile fp ReadMode
        _    <- hGetLine h
        rest <- hGetContents h
        hClose h
        h <- openFile fp WriteMode
        hPutStrLn h $ show exp
        hPutStr h rest
        release ssh


updateDataItem :: SimpleSessionHandler -> SessionId -> SessionItem -> IO ()
updateDataItem ssh sid item@(k,_) = do
        acquire ssh
        let fp = sidPath ssh sid
        h <- openFile fp ReadMode
        exp <- hGetLine h
        raw <- hGetContents h
        hClose h
        let newdata = unlines $ map show $ (filter ((/=k) . fst) $ 
        		map (read :: String -> (Key, (Value, Expires))) $ lines raw) ++ [item]
        writeFile fp $ exp ++ "\n" ++ newdata
        release ssh

deleteDataItem :: SimpleSessionHandler -> SessionId -> Key -> IO ()
deleteDataItem ssh sid k = do
        acquire ssh
        let fp = sidPath ssh sid
        h <- openFile fp ReadMode
        exp <- hGetLine h
        raw <- hGetContents h
        hClose h
        let newdata = unlines $ map show $ filter ((/=k) . fst) $ 
        		map (read :: String -> (Key, (Value, Expires))) $ lines raw
        writeFile fp $ exp ++ "\n" ++ newdata
        release ssh


acquire, release :: SimpleSessionHandler -> IO ()
acquire ssh = takeMVar (lock ssh)
release ssh = putMVar (lock ssh) ()

