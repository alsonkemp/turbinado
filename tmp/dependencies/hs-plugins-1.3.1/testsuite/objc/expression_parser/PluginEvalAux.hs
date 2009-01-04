{-# OPTIONS -fglasgow-exts -fffi #-}

module PluginEvalAux where

import System.Plugins.Make
import System.Plugins.Load
import System.Plugins.Utils

import Foreign.C
import Control.Exception        ( evaluate )
import System.IO
import System.Directory         ( renameFile, removeFile )

symbol = "resource"

evalWithStringResult :: FilePath -> String -> IO String
evalWithStringResult srcFile s = do
  status <- make srcFile ["-Onot"]
  case status of
      MakeFailure err   -> putStrLn "error occured" >> return (show err)
      MakeSuccess _ obj -> load' obj
  where
    load' obj = do
      loadResult <- load obj [] [] symbol
      case loadResult of
        LoadFailure errs -> putStrLn "load error" >> return (show errs)
	LoadSuccess m (rsrc :: String -> IO String) -> do
	  v' <- rsrc s
	  unload m
	  mapM_ removeFile [ obj, replaceSuffix obj ".hi" ]
	  return v'

foreign export ccall evalhaskell_CString :: CString -> CString -> IO CString

evalhaskell_CString :: CString -> CString -> IO CString
evalhaskell_CString filePathCS sCS = do
  s <- peekCString sCS
  filePath <- peekCString filePathCS
  retval <- evalWithStringResult filePath s
  newCString retval

-- vi:sw=2 sts=2

