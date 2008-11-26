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
import Turbinado.View.Monad hiding (doIO)
import Turbinado.View.XML
import Turbinado.Controller.Monad

-- | Create a new store for Code data
addCodeStoreToEnvironment :: Controller ()
addCodeStoreToEnvironment = do e <- get
                               mv <- doIO $ newMVar $ empty
                               put $ e {getCodeStore = Just $ CodeStore mv}

retrieveCode :: CodeType -> CodeLocation -> Controller CodeStatus
retrieveCode ct cl' = do
    e <- get
    let (CodeStore mv) = fromJust $ getCodeStore e
        path  = getDir ct
    cl <- do -- d <- getCurrentDirectory 
             return (addExtension (joinPath $ map normalise [{- d, -} path, dropExtension $ fst cl']) "hs", snd cl')
    debugM $ "  CodeStore : retrieveCode : loading   " ++ (fst cl) ++ " - " ++ (snd cl)
    cmap <- doIO $ takeMVar mv
    let c= lookup cl cmap
    cmap' <- case c of
               Nothing              -> do debugM ((fst cl) ++ " : " ++ (snd cl) ++ " : fresh load")
                                          loadCode ct cmap cl
               Just CodeLoadFailure -> do debugM ((fst cl) ++ " : " ++ (snd cl) ++ " : previous failure; try load") 
                                          loadCode ct cmap cl
               _                    -> do debugM ((fst cl) ++ " : " ++ (snd cl) ++ " : checking reload") 
                                          checkReloadCode ct cmap (fromJust c) cl
    doIO $ putMVar mv cmap'
    -- We _definitely_ have a code entry now, though it may have a MakeFailure
    let c' = lookup cl cmap'
    case c' of
        Nothing                           -> do debugM (fst cl ++ " : Not found in CodeStore") 
                                                return CodeLoadFailure
        Just CodeLoadFailure              -> do debugM (fst cl ++ " : CodeLoadFailure " ) 
                                                return CodeLoadFailure
        Just clc@(CodeLoadController _ _ _) -> do debugM (fst cl ++ " : CodeLoadController " ) 
                                                  return clc  
        Just clv@(CodeLoadView       _ _ _) -> do debugM (fst cl ++ " : CodeLoadView" ) 
                                                  return clv
        
checkReloadCode :: CodeType -> CodeMap -> CodeStatus -> CodeLocation -> Controller CodeMap
checkReloadCode ct cmap CodeLoadFailure cl = error "ERROR: checkReloadCode was called with a CodeLoadFailure"
checkReloadCode ct cmap cstat cl = do
    debugM $ "    CodeStore : checkReloadCode : loading   " ++ (fst cl) ++ " - " ++ (snd cl)
    r <- needReloadCode (fst cl) (getDate cstat)
    case r of
        False -> do debugM $ "    CodeStore : checkReloadCode : No reload neeeded"
                    return cmap
        True  -> do debugM $ "    CodeStore : checkReloadCode : Need reload"
                    loadCode ct cmap cl

        
-- The beast
-- In cases of Merge, Make or Load failures leave the original files in place and log the error
loadCode :: CodeType -> CodeMap -> CodeLocation -> Controller CodeMap
loadCode ct cmap cl = do
    debugM $ "\tCodeStore : loadCode : loading   " ++ (fst cl) ++ " - " ++ (snd cl)
    fe <- doIO $ doesFileExist $ fst cl
    case fe of 
        False -> debugM ("\tFile not found: " ++ fst cl) >> return cmap 
        True  -> mergeCode ct cmap cl
        
mergeCode :: CodeType -> CodeMap -> CodeLocation -> Controller CodeMap
mergeCode ct cmap cl = do
    debugM $ "\tMerging " ++ (fst cl)
    -- d <- getCurrentDirectory
    --debugM $ "  stub " ++ joinPath [normalise d, normalise $ getStub ct]
    ms <- customMergeToDir (joinPath [{-normalise d,-} normalise $ getStub ct]) (fst cl) compiledDir
    case ms of
        MergeFailure err            -> do debugM ("\tMerge error : " ++ (show err))
                                          return $ insert cl CodeLoadFailure cmap
        MergeSuccess NotReq _    _  -> do debugM ("\tMerge success (No recompilation required) : " ++ (fst cl)) 
                                          return cmap
        MergeSuccess _      args fp -> do debugM ("\tMerge success : " ++ (fst cl)) 
                                          makeCode ct cmap cl args fp
        
makeCode :: CodeType -> CodeMap -> CodeLocation -> [Arg] -> FilePath -> Controller CodeMap
makeCode ct cmap cl args fp = do
    ms <- doIO $ makeAll fp (compileArgs++args)
    case ms of
        MakeFailure err       -> do debugM ("\tMake error : " ++ (show err)) 
                                    return (insert cl CodeLoadFailure cmap)
        MakeSuccess NotReq _  -> do debugM ("\tMake success : No recomp required") 
                                    return (insert cl CodeLoadFailure cmap)
        MakeSuccess _      fp -> do debugM ("\tMake success : " ++ fp)
                                    case ct of
                                      CTController -> _loadController ct cmap cl fp
                                      _            -> _loadView       ct cmap cl fp

_loadController :: CodeType -> CodeMap -> CodeLocation -> FilePath -> Controller CodeMap
_loadController ct cmap cl fp = do
    debugM ("loadController : " ++ (fst cl) ++ " : " ++ (snd cl))
    ls <- doIO $ load_ fp [compiledDir] (snd cl)
    case ls of 
        LoadFailure err -> do debugM ("LoadFailure : " ++ (show err)) 
                              return (insert cl CodeLoadFailure cmap)
        LoadSuccess m f -> do debugM ("LoadSuccess : " ++ fst cl )
                              doIO $ unload m
                              t <- doIO $ getClockTime
                              return (insert cl (CodeLoadController f m t) cmap)

_loadView :: CodeType -> CodeMap -> CodeLocation -> FilePath -> Controller CodeMap
_loadView ct cmap cl fp = do
    debugM ("loadView : " ++ (fst cl) ++ " : " ++ (snd cl))
    ls <- doIO $ load_ fp (compiledDir:searchDirs) (snd cl)
    case ls of 
        LoadFailure err -> do debugM ("\tLoadFailure : " ++ (show err)) 
                              return (insert cl CodeLoadFailure cmap)
        LoadSuccess m f -> do debugM ("\tLoadSuccess : " ++ fst cl )
                              doIO $ unload m
                              t <- doIO $ getClockTime
                              return (insert cl (CodeLoadView  f m t) cmap)


-------------------------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------------------------

-- Custom merge function because I don't want to have to use a custom
-- version of Plugins (with HSX enabled)
customMergeToDir :: FilePath -> FilePath -> FilePath -> Controller MergeStatus
customMergeToDir stb src dir = do
    src_exists <- doIO $ doesFileExist src
    stb_exists <- doIO $ doesFileExist stb
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
                src_str <- doIO $ readFile src
                stb_str <- doIO $ readFile stb
                let (stbimps, stbdecls) = span ( not . isPrefixOf "-- SPLIT HERE") $ lines stb_str
                    mrg_str = outTitle ++ (unlines stbimps) ++ src_str ++ (unlines stbdecls)
                doIO $ createDirectoryIfMissing True outDir
                hdl <- doIO $ openFile outFile WriteMode  -- overwrite!
                doIO $ hPutStr hdl mrg_str 
                doIO $ hClose hdl
                return $ MergeSuccess ReComp [] outFile -- must have recreated file
 

needReloadCode :: FilePath -> CodeDate -> Controller Bool
needReloadCode fp fd = do
    fe <- doIO $ doesFileExist fp
    case fe of
        True -> do mt <- doIO $ getModificationTime fp    
                   return $ mt > fd
        False-> return True

snd' :: (a, b, c) -> b
snd' (a,b,c) = b

getDir :: CodeType -> FilePath
getDir ct = case ct of
  CTLayout     -> layoutDir
  CTController -> controllerDir
  CTView       -> viewDir

getStub :: CodeType -> FilePath
getStub ct = case ct of
  CTLayout     -> layoutStub
  CTController -> controllerStub
  CTView       -> viewStub

getDate CodeLoadFailure = error "getDate called with CodeLoadFailure"
getDate (CodeLoadView       _ _ d) = d
getDate (CodeLoadController _ _ d) = d
