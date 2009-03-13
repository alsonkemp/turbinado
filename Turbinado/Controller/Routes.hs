-----------------------------------------------------------------------------
-- |
-- Module      :  Turbinado.Controller.Routes
-- Copyright   :  (c) Alson Kemp 2009
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  Alson Kemp (alson@alsonkemp.com)
-- Stability   :  experimental
-----------------------------------------------------------------------------
module Turbinado.Controller.Routes (
  checkFormats
  ) where

import Data.Maybe
import Network.HTTP.Headers

import Turbinado.Environment.MimeTypes
import Turbinado.Environment.Request
import Turbinado.Environment.Response
import Turbinado.Environment.Settings
import Turbinado.Environment.Types
import Turbinado.Controller.Monad

-- | Automates the process of responding to various file formats
checkFormats:: Controller ()
checkFormats = do f' <- getSetting "format"
                  case f' of
                    Nothing -> return ()
                    Just f  -> do clearLayout
                                  oldAction <- getSetting_u "action"
                                  setSetting "action" (oldAction ++ f)
                                  e <- getEnvironment
                                  let mts = fromJust $ getMimeTypes e
                                      mt  = mimeTypeOf mts f
                                      rsp = fromJust $ getResponse e
                                  case mt of 
                                    Nothing  -> return ()
                                    Just (MimeType s1 s2) -> setResponse $
                                                              replaceHeader
                                                                HdrContentType 
                                                                (s1 ++ "/" ++ s2)
                                                                rsp
