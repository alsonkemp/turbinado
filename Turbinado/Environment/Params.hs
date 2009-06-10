module Turbinado.Environment.Params(
getParam,
getParam_u,
populateParamsAndFiles
) where

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Map as M
import Data.Int (Int64)
import Data.List
import Data.Maybe
import Network.HTTP
import Network.HTTP.Headers
import Network.URI

import Turbinado.Environment.Header
import Turbinado.Environment.Logger
import Turbinado.Environment.Request
import Turbinado.Environment.Types
import Turbinado.Utility.Data

import Codec.MIME.Parse
import Codec.MIME.Type

-- | Attempt to get a Parameter from the Request query string
-- or POST body.
getParam :: (HasEnvironment m) => String -> m (Maybe String)
getParam p = do (Params ps) <- populateParamsAndFiles -- lazy population
                return $ M.lookup p ps

-- | An unsafe version of getParam.  Errors if the key does not exist.
getParam_u :: (HasEnvironment m) => String -> m String
getParam_u p =  do r <- getParam p
                   maybe (error $ "getParam_u : key does not exist - \"" ++ p ++ "\"")
                       return
                       r

populateParamsAndFiles :: (HasEnvironment m) => m Params
populateParamsAndFiles = do e <- getEnvironment
                            case (getParams e) of
                              Just ps' -> return ps'
                              Nothing -> do ct <- getHeader HdrContentType
                                            let rm = rqMethod (fromJust' "Params : getParamsFromBody" $ Turbinado.Environment.Types.getRequest e)
                                                rb = rqBody   (fromJust' "Params : getParamsFromBody" $ Turbinado.Environment.Types.getRequest e)
                                            qsPs <- setParamsFromQueryString -- always process the query string
                                            case rm of
                                              POST -> case ct of
                                                Just "application/x-www-form-urlencoded" -> setParamsFromBody rb qsPs
                                                _                                        -> setParamsFromForm rb qsPs
                                              _    -> return qsPs

-- Functions used by getParam.  Not exported.
setParamsFromQueryString :: (HasEnvironment m) => m Params
setParamsFromQueryString = do e <- getEnvironment
                              let qs = dropWhile (=='?') $ uriQuery $ rqURI (fromJust' "Params : getParamFromQueryString" $ Turbinado.Environment.Types.getRequest e)
                                  ps = Params $ M.fromList $ qsDecode qs
                              setEnvironment $ e {getParams = Just ps}
                              return ps

setParamsFromBody :: (HasEnvironment m) => String -> Params -> m Params
setParamsFromBody s (Params qsps) = do e <- getEnvironment
                                       let ps = Params $ M.union (M.fromList $ qsDecode s) qsps
                                       setEnvironment $ e {getParams = Just ps}
                                       return ps

setParamsFromForm :: (HasEnvironment m) => String -> Params -> m Params
setParamsFromForm s qsps = multipartDecode s qsps

-- LIFTED FROM THE CGI PACKAGE

-- | Gets the name-value pairs from urlencoded data.
qsDecode :: String -> [(String,String)]
qsDecode "" = []
qsDecode s = (urlDecode n, urlDecode (drop 1 v)) : qsDecode (drop 1 rs)
      where (nv,rs) = break (=='&') s
            (n,v) = break (=='=') nv

multipartDecode :: (HasEnvironment m) => String -> Params -> m Params
multipartDecode s qsps = do hd <- getHeader_u HdrContentType
                            errorM $ "hd: " ++ hd
                            let ms = parseMIMEBody [("content-type", hd)] s
                                (fs, ps) = extractParamsAndFiles ms
                                (Params qsps') = qsps
                                ps' = Params $ M.union ps qsps'
                            errorM $ "qsps: " ++  show qsps'
                            errorM $ "ps: " ++   show ps
                            errorM $ "ps': " ++  show (M.union ps qsps')
                            e <- getEnvironment
                            setEnvironment $ e { getFiles = Just $ Files fs, getParams = Just ps'}
                            return ps'


{-
First draft:

If there is a disposition we could have a file, and we look at the content, if there is one single content we add it into the Files Map.
If there are more than one Files we run the function for each file. 
-}
extractParamsAndFiles :: MIMEValue -> (M.Map String MIMEValue, M.Map String String)
extractParamsAndFiles mi = worker (M.empty, M.empty) mi
  where worker :: (M.Map String MIMEValue, M.Map String String) -> MIMEValue -> (M.Map String MIMEValue, M.Map String String)
        worker (fs,ps) mv = if (isSingle mv)
                              then case (getFileAndName $ dispParams $ fromJust' "MIMEValue has no Dispositions" $ mime_val_disp mv) of
                                     (True,  nm) -> (M.insert nm mv fs,ps)
                                     (False, nm) -> (fs, M.insert nm (_getContent mv) ps)
                              else foldl worker (fs, ps) (_getContents mv)

_getContent mv = case (mime_val_content mv) of
                   Single c -> c
                   _        -> error "Turbinado.Environment.Params.getContent: called with a Multi Content"

_getContents mv = case (mime_val_content mv) of
                    Multi c  -> c
                    _        -> error "Turbinado.Environment.Params.getContents: called with a Single Content"

isSingle :: MIMEValue -> Bool
isSingle mv = case (mime_val_content mv) of
               Single _ -> True
               _        -> False

getFileAndName :: [DispParam] -> (Bool, String)
getFileAndName ds = worker (False, "no-name-found") ds
  where worker (b, n) [] = (b, n)
        worker (_, n) ((Filename _):ds') = worker (True, n) ds'
        worker (b, _) ((Name n'):ds')    = worker (b,   n') ds'

