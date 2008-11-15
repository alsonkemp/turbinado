-----------------------------------------------------------------------------
-- |
-- Module      :  Turbinado.HTML
-- Copyright   :  (c) Niklas Broberg, Jeremy Shaw 2008
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Niklas Broberg, nibro@cs.chalmers.se
-- Stability   :  experimental
-- Portability :  Haskell 98
--
-- Attempt to render XML as well-formed HTML 4.01:
--  * no short tags are used, e.g., <script></script> instead of <script />
--  * the end tag is forbidden for some elements, for these we:
--    * render only the open tag, e.g., <br>
--    * throw an error if the tag contains children
--  * optional end tags are always rendered
--
-- Currently no validation is performed.
-----------------------------------------------------------------------------
module Turbinado.View.HTML (
                 -- * Functions
                  renderAsHTML
                , htmlEscapeChars
                ) where

import Data.List
import Turbinado.View.XML
import Turbinado.View.XML.PCDATA(escaper)

-- | Pretty-prints HTML values.
-- FIXME: also verify that the domain is correct
-- FIXME: what to do if a namespace is encountered
--
-- Error Handling:
--
-- Some tags (such as img) can not contain children in HTML. However,
-- there is nothing to stop the caller from passing in XML which
-- contains an img tag with children. There are three basic ways to
-- handle this:
--
--  1. drop the bogus children silently
--  2. call 'error' / raise an exception
--  3. render the img tag with children -- even though it is invalid
--
-- Currently we are taking approach #3, since no other attempts to
-- validate the data are made in this function. Instead, you can run
-- the output through a full HTML validator to detect the errors.
--
-- #1 seems like a poor choice, since it makes is easy to overlook the
-- fact that data went missing.
--
-- We could raising errors, but you have to be in the IO monad to
-- catch them. Also, you have to use evaluate if you want to check for
-- errors. This means you can not start sending the page until the
-- whole page has been rendered. And you have to store the whole page
-- in RAM at once. Similar problems occur if we return Either
-- instead. We mostly care about catching errors and showing them in
-- the browser during testing, so perhaps this can be configurable.
--
-- Another solution would be a compile time error if an empty-only
-- tag contained children.
renderAsHTML :: XML -> String
renderAsHTML xml = renderAsHTML' 0 xml ""

data TagType = Open | Close

renderAsHTML' :: Int -> XML -> ShowS
renderAsHTML' _ (CDATA needsEscape cd) = showString (if needsEscape then (escaper htmlEscapeChars cd) else cd)
renderAsHTML' n elm@(Element name@(Nothing,nm) attrs children) 
    | nm == "area"  = renderTagEmpty children
    | nm == "base"  = renderTagEmpty children
    | nm == "br"        = renderTagEmpty children
    | nm == "col"       = renderTagEmpty children
    | nm == "hr"        = renderTagEmpty children
    | nm == "img"       = renderTagEmpty children
    | nm == "input"     = renderTagEmpty children
    | nm == "link"      = renderTagEmpty children
    | nm == "meta"      = renderTagEmpty children
    | nm == "param"     = renderTagEmpty children
    | nm == "script"    = renderElement n (Element name attrs (map asCDATA children))
    | nm == "style"     = renderElement n (Element name attrs (map asCDATA children))
    where
      renderTagEmpty [] = renderTag Open n name attrs
      renderTagEmpty _ = renderElement n elm -- ^ this case should not happen in valid HTML
      -- for and script/style, render text in element as CDATA not PCDATA
      asCDATA :: XML -> XML
      asCDATA (CDATA _ cd) = (CDATA False cd)
      asCDATA o = o -- ^ this case should not happen in valid HTML
renderAsHTML' n e = renderElement n e

renderElement n (Element name attrs children) =
        let open  = renderTag Open n name attrs 
            cs    = renderChildren n children 
            close = renderTag Close n name []
         in open . cs . close
  where renderChildren :: Int -> Children -> ShowS
        renderChildren n' cs = foldl (.) id $ map (renderAsHTML' (n'+2)) cs

renderTag :: TagType -> Int -> Name -> Attributes -> ShowS 
renderTag typ n name attrs = 
        let (start,end) = case typ of
                           Open   -> (showChar '<', showChar '>')
                           Close  -> (showString "</", showChar '>')
            nam = showName name
            as  = renderAttrs attrs
         in start . nam . as . end

  where renderAttrs :: Attributes -> ShowS
        renderAttrs [] = nl
        renderAttrs attrs' = showChar ' ' . ats . nl
          where ats = foldl (.) id $ intersperse (showChar ' ') $ fmap renderAttr attrs'


        renderAttr :: Attribute -> ShowS
        renderAttr (MkAttr (nam, (Value needsEscape val))) = showName nam . showChar '=' . renderAttrVal (if needsEscape then (escaper htmlEscapeChars val) else val)

        renderAttrVal :: String -> ShowS
        renderAttrVal s = showChar '\"' . showString s . showChar '\"'

        showName (Nothing, s) = showString s
        showName (Just d, s)  = showString d . showChar ':' . showString s

        nl = showChar '\n' . showString (replicate n ' ')

-- This list should be extended.
htmlEscapeChars :: [(Char, String)]
htmlEscapeChars = [
  ('&', "amp" ),
  -- ('\"',  "quot"  ),
  ('<', "lt"  ),
  ('>', "gt"  )
  ]
