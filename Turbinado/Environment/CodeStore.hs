module Turbinado.Environment.CodeStore (
    addCodeStoreToEnvironment,
    getCodeStore,
    setCodeStore,
    CodeType (..),
    retrieveCode,
    CodeStore (..),
    CodeMap,
    CodeStatus (..)
    ) where

import Control.Concurrent.MVar
import Control.Exception ( catch, throwIO )
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
import Turbinado.Environment
import Turbinado.Environment.Request
import Turbinado.Environment.Response
import Turbinado.View.Monad
import Turbinado.View.XML
import Turbinado.Controller.Monad

type CodeDate      = ClockTime
type Function      = String
type CodeLocation  = (FilePath, Function)

data CodeStore  = CodeStore (MVar CodeMap)
  deriving Typeable
type CodeMap    = Map CodeLocation CodeStatus
data CodeStatus = CodeLoadFailure | 
                  CodeLoadController (Controller ()) Module CodeDate |
                  CodeLoadView       (View XML     ) Module CodeDate

-- | Create a new store for Code data
addCodeStoreToEnvironment :: EnvironmentFilter
addCodeStoreToEnvironment e = do mv <- newMVar $ empty
                                 setCodeStore (CodeStore mv) e

codeStoreKey = "codestore"

getCodeStore :: Environment -> CodeStore
getCodeStore = getKey codeStoreKey

setCodeStore :: CodeStore -> EnvironmentFilter
setCodeStore req = setKey codeStoreKey req


data CodeType = CTView | CTController | CTLayout

retrieveCode :: Environment -> CodeType -> CodeLocation -> IO CodeStatus
retrieveCode e ct cl' = do
    let (CodeStore mv) = getCodeStore e
        path  = getDir ct
    cl <- do -- d <- getCurrentDirectory 
             return (addExtension (joinPath $ map normalise [{- d, -} path, dropExtension $ fst cl']) "hs", snd cl')
    debugM e $ "  CodeStore : retrieveCode : loading   " ++ (fst cl) ++ " - " ++ (snd cl)
    cmap <- takeMVar mv
    let c= lookup cl cmap
    cmap' <- case c of
               Nothing              -> do debugM e ((fst cl) ++ " : " ++ (snd cl) ++ " : fresh load")
                                          loadCode e ct cmap cl
               Just CodeLoadFailure -> do debugM e ((fst cl) ++ " : " ++ (snd cl) ++ " : previous failure; try load") 
                                          loadCode e ct cmap cl
               _                    -> do debugM e ((fst cl) ++ " : " ++ (snd cl) ++ " : checking reload") 
                                          checkReloadCode e ct cmap (fromJust c) cl
    putMVar mv cmap'
    -- We _definitely_ have a code entry now, though it may have a MakeFailure
    let c' = lookup cl cmap'
    case c' of
        Nothing                           -> do debugM e (fst cl ++ " : Not found in CodeStore") 
                                                return CodeLoadFailure
        Just CodeLoadFailure              -> do debugM e (fst cl ++ " : CodeLoadFailure " ) 
                                                return CodeLoadFailure
        Just clc@(CodeLoadController _ _ _) -> do debugM e (fst cl ++ " : CodeLoadController " ) 
                                                  return clc  
        Just clv@(CodeLoadView       _ _ _) -> do debugM e (fst cl ++ " : CodeLoadView" ) 
                                                  return clv
        
checkReloadCode :: Environment -> CodeType -> CodeMap -> CodeStatus -> CodeLocation -> IO CodeMap
checkReloadCode e ct cmap CodeLoadFailure cl = error "ERROR: checkReloadCode was called with a CodeLoadFailure"
checkReloadCode e ct cmap cstat cl = do
    debugM e $ "    CodeStore : checkReloadCode : loading   " ++ (fst cl) ++ " - " ++ (snd cl)
    r <- needReloadCode e (fst cl) (getDate cstat)
    case r of
        False -> do debugM e $ "    CodeStore : checkReloadCode : No reload neeeded"
                    return cmap
        True  -> do debugM e $ "    CodeStore : checkReloadCode : Need reload"
                    loadCode e ct cmap cl

        
-- The beast
-- In cases of Merge, Make or Load failures leave the original files in place and log the error
loadCode :: Environment -> CodeType -> CodeMap -> CodeLocation -> IO CodeMap
loadCode e ct cmap cl = do
    debugM e $ "\tCodeStore : loadCode : loading   " ++ (fst cl) ++ " - " ++ (snd cl)
    fe <- doesFileExist $ fst cl
    case fe of 
        False -> debugM e ("\tFile not found: " ++ fst cl) >> return cmap 
        True  -> mergeCode e ct cmap cl
        
mergeCode :: Environment -> CodeType -> CodeMap -> CodeLocation -> IO CodeMap
mergeCode e ct cmap cl = do
    debugM e $ "\tMerging " ++ (fst cl)
    -- d <- getCurrentDirectory
    --debugM e $ "  stub " ++ joinPath [normalise d, normalise $ getStub ct]
    ms <- customMergeToDir (joinPath [{-normalise d,-} normalise $ getStub ct]) (fst cl) compiledDir
    case ms of
        MergeFailure err            -> do debugM e ("\tMerge error : " ++ (show err))
                                          return $ insert cl CodeLoadFailure cmap
        MergeSuccess NotReq _    _  -> do debugM e ("\tMerge success (No recompilation required) : " ++ (fst cl)) 
                                          return cmap
        MergeSuccess _      args fp -> do debugM e ("\tMerge success : " ++ (fst cl)) 
                                          makeCode e ct cmap cl args fp
        
makeCode :: Environment -> CodeType -> CodeMap -> CodeLocation -> [Arg] -> FilePath -> IO CodeMap
makeCode e ct cmap cl args fp = do
    ms <- makeAll fp (compileArgs++args)
    case ms of
        MakeFailure err       -> do debugM e ("\tMake error : " ++ (show err)) 
                                    return (insert cl CodeLoadFailure cmap)
        MakeSuccess NotReq _  -> do debugM e ("\tMake success : No recomp required") 
                                    return (insert cl CodeLoadFailure cmap)
        MakeSuccess _      fp -> do debugM e ("\tMake success : " ++ fp)
                                    case ct of
                                      CTController -> _loadController e ct cmap cl fp
                                      _            -> _loadView       e ct cmap cl fp

_loadController :: Environment -> CodeType -> CodeMap -> CodeLocation -> FilePath -> IO CodeMap
_loadController e ct cmap cl fp = do
    debugM e ("loadController : " ++ (fst cl) ++ " : " ++ (snd cl))
    ls <- load_ fp [compiledDir] (snd cl)
    case ls of 
        LoadFailure err -> do debugM e ("LoadFailure : " ++ (show err)) 
                              return (insert cl CodeLoadFailure cmap)
        LoadSuccess m f -> do debugM e ("LoadSuccess : " ++ fst cl )
                              unload m
                              t <- getClockTime
                              return (insert cl (CodeLoadController f m t) cmap)

_loadView :: Environment -> CodeType -> CodeMap -> CodeLocation -> FilePath -> IO CodeMap
_loadView e ct cmap cl fp = do
    debugM e ("loadView : " ++ (fst cl) ++ " : " ++ (snd cl))
    ls <- load_ fp (compiledDir:searchDirs) (snd cl)
    case ls of 
        LoadFailure err -> do debugM e ("\tLoadFailure : " ++ (show err)) 
                              return (insert cl CodeLoadFailure cmap)
        LoadSuccess m f -> do debugM e ("\tLoadSuccess : " ++ fst cl )
                              unload m
                              t <- getClockTime
                              return (insert cl (CodeLoadView  f m t) cmap)


-------------------------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------------------------

-- Custom merge function because I don't want to have to use a custom
-- version of Plugins (with HSX enabled)
customMergeToDir :: FilePath -> FilePath -> FilePath -> IO MergeStatus
customMergeToDir stb src dir = do
    src_exists <- doesFileExist src
    stb_exists <- doesFileExist stb
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
                src_str <- readFile src
                stb_str <- readFile stb
                let (stbimps, stbdecls) = span ( not . isPrefixOf "-- SPLIT HERE") $ lines stb_str
                    mrg_str = outTitle ++ (unlines stbimps) ++ src_str ++ (unlines stbdecls)
                createDirectoryIfMissing True outDir
                hdl <- openFile outFile WriteMode  -- overwrite!
                hPutStr hdl mrg_str 
                hClose hdl
                return $ MergeSuccess ReComp [] outFile -- must have recreated file
 

needReloadCode :: Environment -> FilePath -> CodeDate -> IO Bool
needReloadCode e fp fd = do
    fe <- doesFileExist fp
    case fe of
        True -> do mt <- getModificationTime fp    
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
