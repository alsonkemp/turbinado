module Turbinado.View.Helpers.Misc ( 
  breadCrumbs
  ) where

import Data.List
import Data.Maybe
import qualified Network.URI as URI
import qualified Network.HTTP as HTTP
import System.FilePath

import Turbinado.Environment.Types
import Turbinado.Environment.Request
import Turbinado.View.Monad
import Turbinado.View.HTML

breadCrumbs :: VHtml
breadCrumbs = do e <- getEnvironment
                 let r  = fromJust $ getRequest e
                     ps = tail $ splitDirectories $ URI.uriPath $ rqURI r
                 (tag "div"![strAttr "class" "breadcrumbs"]) << (stringToVHtml $ concat $ intersperse " : " ps)
