module Turbinado.Environment.Files(
  getFile,
  getFile_u,
  getFileContent,
  getFileDecode
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Maybe
import Network.HTTP
import Network.HTTP.Headers
import Network.URI
import Codec.MIME.Type
import Codec.MIME.Decode

import Turbinado.Environment.Params
import Turbinado.Environment.Types
import Turbinado.Utility.Data

-- | Attempt to get a File from the POST
getFile :: (HasEnvironment m) => String -> m (Maybe MIMEValue)
getFile f = do populateParamsAndFiles
               e <- getEnvironment
               let Files fs = fromJust' "Turbinado.Environment.Files.getFile: Files is Nothing" $ getFiles e
               return $ M.lookup f fs

-- | An unsafe version of getFile.  Errors if the key does not exist.
getFile_u :: (HasEnvironment m) => String -> m MIMEValue
getFile_u f =  do r <- getFile f
                  maybe (error $ "getFile_u : key does not exist - \"" ++ f ++ "\"")
                         return
                         r

getFileContent :: MIMEValue -> String
getFileContent mv = case (mime_val_content mv) of
                      Single c -> c
                      _        -> error "Turbinado.Environment.Params.getContent: called with a Multi Content"

getFileDecode :: MIMEValue -> String
getFileDecode mv = 
  case (mime_val_content mv) of
    --Single c -> decodeBody "base64" c
    Single c -> decodeWords c
    _        -> error "Turbinado.Environment.Params.getContent: called with a Multi Content" 
