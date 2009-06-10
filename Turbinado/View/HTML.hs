module Turbinado.View.HTML where

import Control.Monad
import Turbinado.View.Monad
import qualified Text.XHtml.Strict as X

type VHtml = View X.Html

class VHTML a where
  toVHtml :: a -> VHtml
  toVHtmlFromList :: [a] -> VHtml
  toVHtmlFromList xs = do xs' <- mapM toVHtml xs
                          return $ X.concatHtml xs'

instance VHTML X.Html where
      toVHtml    = return

instance VHTML VHtml where
      toVHtml v = v

instance VHTML Char where
      toVHtml       a = return $ X.toHtml [a]
      toVHtmlFromList = return . X.toHtmlFromList

instance (VHTML a) => VHTML [a] where
      toVHtml = toVHtmlFromList

instance VHTML a => VHTML (Maybe a) where
      toVHtml m = maybe noVHtml toVHtml m


class ADDATTRS a where
      (!) :: a -> [X.HtmlAttr] -> a

instance (ADDATTRS b) => ADDATTRS (a -> b) where
      fn ! attr = \ arg -> fn arg ! attr

instance ADDATTRS VHtml where
      vh ! attr = do vh' <- vh
                     return $ vh' X.! attr

-- 
-- * Html primitives and basic combinators
--

-- | Put something inside an HTML element.
(<<) :: (VHTML a) => 
        (VHtml -> b) -- ^ Parent
     -> a -- ^ Child
     -> b
fn << arg = fn (toVHtml arg) 


concatVHtml :: (VHTML a) => [a] -> VHtml
concatVHtml as = do hs <- sequence $ map toVHtml as
                    return $ X.concatHtml hs 

-- | Create a piece of HTML which is the concatenation
--   of two things which can be made into HTML.
(+++) :: (VHTML a,VHTML b) => a -> b -> VHtml
a +++ b = do a' <- toVHtml a
             b' <- toVHtml b
             return $ a' X.+++ b'

-- | An empty piece of HTML.
noVHtml :: VHtml
noVHtml = return $ X.noHtml

-- | Constructs an element with a custom name.
tag :: String -- ^ Element name
    -> VHtml -- ^ Element contents
    -> VHtml
tag str htmls = do hs <- htmls
                   return $ X.tag str hs

-- | Constructs an element with a custom name, and
--   without any children.
itag :: String -> VHtml
itag str = tag str noVHtml
stringToVHtml :: String -> VHtml
stringToVHtml s = return $ X.stringToHtml s

emptyAttr = X.emptyAttr
intAttr  = X.intAttr
strAttr = X.strAttr
htmlAttr  = X.htmlAttr

--prettyHtml = X.prettyHtml
