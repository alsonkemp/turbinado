module Turbinado.Environment.CodeStore (
    addCodeStoreToEnvironment,
    retrieveCode,
    ) where

import Control.Concurrent.MVar
import Control.Exception ( catch, throwIO)
import Control.Monad ( when, foldM)
import Data.Map hiding (map)
import Data.List (isPrefixOf, intersperse)
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Data.Typeable
import qualified Network.HTTP as HTTP
import Prelude hiding (lookup,catch)
import System.Directory
import System.FilePath
import System.IO
import System.Plugins
import System.Plugins.Utils
import System.Time

import Config.Master

import qualified Turbinado.Server.Exception as Ex
import Turbinado.Environment.Logger
import Turbinado.Environment.Types
import Turbinado.Environment.Request
import Turbinado.Environment.Response
import Turbinado.Utility.Data
import Turbinado.View.Monad hiding (liftIO)
import Turbinado.View.XML
import Turbinado.Controller.Monad

-- | Create a new store for Code data
addCodeStoreToEnvironment :: (HasEnvironment m) => m ()
addCodeStoreToEnvironment = do e <- getEnvironment
                               let cm = empty
                               mv <- liftIO $ newMVar cm
                               setEnvironment $ e {getCodeStore = Just $ CodeStore mv}

-- | This function attempts to pull a function from a pre-loaded cache or, if
-- the function doesn't exist or is out-of-date, loads the code from disk.
retrieveCode :: (HasEnvironment m) => CodeType -> CodeLocation -> m CodeStatus
retrieveCode ct cl' = do
    e <- getEnvironment
    let (CodeStore mv) = fromJust' "CodeStore: retrieveCode" $ getCodeStore e
        path  = getDir ct
    cl <- return (addExtension (joinPath $ map normalise [path, dropExtension $ fst cl']) "hs", snd cl')
    debugM $ "  CodeStore : retrieveCode : loading   " ++ (fst cl) ++ " - " ++ (snd cl)
    cmap <- liftIO $ takeMVar mv
    let c= lookup cl cmap
    cmap' <- case c of
               Nothing                  -> do debugM ((fst cl) ++ " : " ++ (snd cl) ++ " : fresh load")
                                              loadCode ct cmap cl
               Just (CodeLoadFailure _) -> do debugM ((fst cl) ++ " : " ++ (snd cl) ++ " : previous failure; try load") 
                                              loadCode ct cmap cl
               _                        -> do debugM ((fst cl) ++ " : " ++ (snd cl) ++ " : checking reload") 
                                              checkReloadCode ct cmap (fromJust' "CodeStore: retrieveCode2" c) cl
    liftIO $ putMVar mv cmap'
    -- We _definitely_ have a code entry now, though it may have a MakeFailure
    let c' = lookup cl cmap'
    case c' of
        Nothing                             -> do debugM (fst cl ++ " : Not found in CodeStore") 
                                                  return  CodeLoadMissing
        Just CodeLoadMissing                -> do debugM (fst cl ++ " : Not found in CodeStore") 
                                                  return  CodeLoadMissing
        Just (CodeLoadFailure e)            -> do debugM (fst cl ++ " : CodeLoadFailure " ) 
                                                  return (CodeLoadFailure e)
        Just clc@(CodeLoadController   _ _) -> do debugM (fst cl ++ " : CodeLoadController " ) 
                                                  return clc  
        Just clv@(CodeLoadView         _ _) -> do debugM (fst cl ++ " : CodeLoadView" ) 
                                                  return clv
        Just clc@(CodeLoadComponentController   _ _) -> do debugM (fst cl ++ " : CodeLoadComponentController " ) 
                                                           return clc  
        Just clv@(CodeLoadComponentView         _ _) -> do debugM (fst cl ++ " : CodeLoadComponentView" ) 
                                                           return clv
        
-- | Checks to see if the file exists and if the file is newer than the loaded function.  If the 
-- code needs to be reloaded, then 'loadCode' will be called.
checkReloadCode :: (HasEnvironment m) => CodeType -> CodeMap -> CodeStatus -> CodeLocation -> m CodeMap
checkReloadCode ct cmap (CodeLoadFailure e) cl = error "ERROR: checkReloadCode was called with a CodeLoadFailure"
checkReloadCode ct cmap cstat cl = do
    debugM $ "    CodeStore : checkReloadCode : loading   " ++ (fst cl) ++ " - " ++ (snd cl)
    (exists, reload) <- needReloadCode (fst cl) (getDate cstat)
    case (exists, reload) of
        (False, _)    -> do debugM $ "    CodeStore : checkReloadCode : Code missing"
                            return $ insert cl CodeLoadMissing cmap
        (True, False) -> do debugM $ "    CodeStore : checkReloadCode : No reload neeeded"
                            return cmap
        (True, True)  -> do debugM $ "    CodeStore : checkReloadCode : Need reload"
                            loadCode ct cmap cl
  where needReloadCode :: (HasEnvironment m) => FilePath -> CodeDate -> m (Bool, Bool)
        needReloadCode fp fd = do
            fe <- liftIO $ doesFileExist fp
            case fe of
                True -> do TOD mt _ <- liftIO $ getModificationTime fp    
                           return $ (True, fromIntegral mt > utcTimeToPOSIXSeconds fd)
                False-> return (False, True)

        
--------------------------------------------------------------------------
-- The beast
--------------------------------------------------------------------------
       
-- | Begins the code load process, which comprises merging the code with
-- the appropriate Stub (in Turbinado/Stubs) to the tmp/compiled directory,
-- making the code, and loading it. 
loadCode :: (HasEnvironment m) => CodeType -> CodeMap -> CodeLocation -> m CodeMap
loadCode ct cmap cl = do
    debugM $ "\tCodeStore : loadCode : loading   " ++ (fst cl) ++ " - " ++ (snd cl)
    mergeCode ct cmap cl
        
-- | Merges the application code with the appropriate Stub, places the merged
-- file into @tmp/compiled@, then calls 'makeCode'.
mergeCode :: (HasEnvironment m) => CodeType -> CodeMap -> CodeLocation -> m CodeMap
mergeCode ct cmap cl = do
    debugM $ "\tMerging " ++ (fst cl)
    ms <- customMergeToDir (joinPath [normalise $ getStub ct]) (fst cl) compiledDir
    case ms of
        MergeFailure err            -> do debugM ("\tMerge error : " ++ (show err))
                                          return $ insert cl (CodeLoadFailure $ unlines err) cmap
        MergeSuccess NotReq _    _  -> do debugM ("\tMerge success (No recompilation required) : " ++ (fst cl)) 
                                          return cmap
        MergeSuccess _      args fp -> do debugM ("\tMerge success : " ++ (fst cl)) 
                                          makeCode ct cmap cl args fp


-- | Attempt to make the code, then call the loader appropriate for the 'CodeType' (e.g. @View@ -> '_loadView').
makeCode :: (HasEnvironment m) => CodeType -> CodeMap -> CodeLocation -> [Arg] -> FilePath -> m CodeMap
makeCode ct cmap cl args fp = do
    ms <- liftIO $ makeAll fp (compileArgs++args)
    case ms of
            MakeFailure err       -> do debugM ("\tMake error : " ++ (show err))
                                        return (insert cl (CodeLoadFailure $ unlines err) cmap)
            MakeSuccess NotReq _  -> do debugM ("\tMake success : No recomp required")
                                        return cmap
            MakeSuccess _      fp -> do debugM ("\tMake success : " ++ fp)
                                        case ct of
                                          CTLayout                -> _loadView ct cmap cl args fp
                                          CTView                  -> _loadView ct cmap cl args fp
                                          CTComponentView         -> _loadView ct cmap cl args fp
                                          CTController            -> _loadController ct cmap cl args fp
                                          CTComponentController   -> _loadController ct cmap cl args fp

-- | Attempt to load the code and return the 'CodeMap' with the newly loaded code in it.  This
-- function is specialized for Views.
_loadView :: (HasEnvironment m) => CodeType -> CodeMap -> CodeLocation -> [Arg] -> FilePath -> m CodeMap
_loadView ct cmap cl args fp = do
    debugM ("_load : " ++ (show ct) ++ " : " ++ (fst cl) ++ " : " ++ (snd cl))
    ls <- liftIO $ load_ fp [compiledDir] (snd cl)
    case ls of 
                LoadFailure err -> do debugM ("LoadFailure : " ++ (show err))
                                      return (insert cl (CodeLoadFailure $ unlines err) cmap)
                LoadSuccess m f -> do debugM ("LoadSuccess : " ++ fst cl )
                                      liftIO $ unload m
                                      t <- liftIO $ getCurrentTime
                                      case ct of
                                        CTLayout              -> return (insert cl (CodeLoadView f t) cmap)
                                        CTView                -> return (insert cl (CodeLoadView f t) cmap)
                                        CTComponentView       -> return (insert cl (CodeLoadComponentView f t) cmap)
                                        _                     -> error $ "_loadView: passed an invalid CodeType (" ++ (show ct)

-- | Attempt to load the code and return the 'CodeMap' with the newly loaded code in it.  This
-- function is specialized for Controllers.
_loadController :: (HasEnvironment m) => CodeType -> CodeMap -> CodeLocation -> [Arg] -> FilePath -> m CodeMap
_loadController ct cmap cl args fp = do
    debugM ("_load : " ++ (show ct) ++ " : " ++ (fst cl) ++ " : " ++ (snd cl))
    ls <- liftIO $ load_ fp [compiledDir] (snd cl)
    case ls of
            LoadFailure err -> do debugM ("LoadFailure : " ++ (show err))
                                  return (insert cl (CodeLoadFailure $ unlines err) cmap)
            LoadSuccess m f -> do debugM ("LoadSuccess : " ++ fst cl )
                                  liftIO $ unload m
                                  t <- liftIO $ getCurrentTime
                                  case ct of
                                    CTController          -> return (insert cl (CodeLoadController f t) cmap)
                                    CTComponentController -> return (insert cl (CodeLoadComponentController f t) cmap)
                                    _                     -> error $ "_loadController: passed an invalid CodeType (" ++ (show ct) ++ ")"


-------------------------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------------------------
-- | Custom merge function because I don't want to have to use a custom
-- version of Plugins (with HSX enabled)
customMergeToDir :: (HasEnvironment m) => FilePath -> FilePath -> FilePath -> m MergeStatus
customMergeToDir stb src dir = do
    src_exists <- liftIO $ doesFileExist src
    stb_exists <- liftIO $ doesFileExist stb
    let outFile = joinPath [dir, src]
        outDir  = joinPath $ init $ splitDirectories outFile
        outMod  = concat $ intersperse "." $ splitDirectories $ dropExtension src
        outTitle = "module " ++ outMod ++ " where \n\n"
    case (src_exists, stb_exists) of
        (False, _) -> return $ 
                MergeFailure ["Source file does not exist : "++src]
        (_, False) -> return $ 
                MergeFailure ["Source file does not exist : "++stb]
        _          -> do
                src_str <- liftIO $ readFile src
                stb_str  <- liftIO $ readFile stb
                -- Check to see whether the file start with "module ".  If so, the user
                -- should already have added the require preamble.  Otherwise, merge the stub.
                let mrg_str = case src_str of
                                ('m':'o':'d':'u':'l':'e':' ':_) -> src_str
                                _ -> let (stbimps, stbdecls) = span ( not . isPrefixOf "-- SPLIT HERE") $ lines stb_str
                                     in outTitle ++ (unlines stbimps) ++ src_str ++ (unlines stbdecls)
                liftIO $ createDirectoryIfMissing True outDir
                hdl <- liftIO $ openFile outFile WriteMode  -- overwrite!
                liftIO $ hPutStr hdl mrg_str 
                liftIO $ hClose hdl
                return $ MergeSuccess ReComp [] outFile -- must have recreated file
 


-- | Given a 'CodeType', return the base path to the code (e.g. @CTController@ -> @App/Controllers@).
getDir :: CodeType -> FilePath
getDir ct = case ct of
  CTLayout         -> layoutDir
  CTController     -> controllerDir
  CTView           -> viewDir
  CTComponentController -> componentControllerDir
  CTComponentView       -> componentViewDir

getStub :: CodeType -> FilePath
getStub ct = case ct of
  CTLayout         -> layoutStub
  CTController     -> controllerStub
  CTView           -> viewStub
  CTComponentController -> controllerStub
  CTComponentView       -> viewStub

getDate (CodeLoadMissing) = error "getDate called with CodeLoadMissing"
getDate (CodeLoadFailure e) = error "getDate called with CodeLoadFailure"
getDate (CodeLoadView       _ d) = d
getDate (CodeLoadController _ d) = d
getDate (CodeLoadComponentView       _ d) = d
getDate (CodeLoadComponentController _ d) = d
