-----------------------------------------------------------------------------
-- |
-- Module      :  Turbinado.View.XML
-- Copyright   :  (c) Alson Kemp 2008, Niklas Broberg 2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  Alson Kemp (alson@alsonkemp.com)
-- Stability   :  experimental
-- Portability :  Haskell 98
--
-- Datatypes and type classes comprising the basic model behind
-- the scenes of Haskell Server Views tags.
-----------------------------------------------------------------------------
module Turbinado.View.XML (
        -- * The 'XML' datatype
        XML(..), 
        Domain, 
        Name, 
        Attributes, 
        Children,
        pcdata,
        cdata,
        -- * The Attribute type
        Attribute(..),
        AttrValue(..),
        attrVal, pAttrVal,
        -- * Functions
        renderXML,
        isElement, isCDATA
        ) where

import Data.List (intersperse)

import Turbinado.View.XML.PCDATA (escape)
---------------------------------------------------------------
-- Data types

type Domain = Maybe String
type Name = (Domain, String)
type Attributes = [Attribute]
type Children = [XML]

-- | The XML datatype representation. Is either an Element or CDATA.
data XML = Element Name Attributes Children
         | CDATA Bool String
  deriving Show

{- instance Show XML where
 show = renderXML -}

-- | Test whether an XML value is an Element or CDATA
isElement, isCDATA :: XML -> Bool
isElement (Element {}) = True
isElement _ = False
isCDATA = not . isElement

-- | Embeds a string as a CDATA XML value.
cdata , pcdata :: String -> XML
cdata  = CDATA False
pcdata = CDATA True

---------------------------------------------------------------
-- Attributes

newtype Attribute = MkAttr (Name, AttrValue)
  deriving Show

-- | Represents an attribue value.
data AttrValue = Value Bool String

-- | Create an attribue value from a string.
attrVal, pAttrVal :: String -> AttrValue
attrVal  = Value False
pAttrVal = Value True

instance Show AttrValue where
 show (Value _ str) = str

------------------------------------------------------------------
-- Rendering

-- TODO: indents are incorrectly calculated

-- | Pretty-prints XML values.
renderXML :: XML -> String
renderXML xml = renderXML' 0 xml ""

data TagType = Open | Close | Single

renderXML' :: Int -> XML -> ShowS
renderXML' _ (CDATA needsEscape cd) = showString (if needsEscape then escape cd else cd)
renderXML' n (Element name attrs []) = renderTag Single n name attrs
renderXML' n (Element name attrs children) =
        let open  = renderTag Open n name attrs 
            cs    = renderChildren n children 
            close = renderTag Close n name []
         in open . cs . close

  where renderChildren :: Int -> Children -> ShowS
        renderChildren n' cs = foldl (.) id $ map (renderXML' (n'+2)) cs

                
renderTag :: TagType -> Int -> Name -> Attributes -> ShowS 
renderTag typ n name attrs = 
        let (start,end) = case typ of
                           Open   -> (showChar '<', showChar '>')
                           Close  -> (showString "</", showChar '>')
                           Single -> (showChar '<', showString "/>")
            nam = showName name
            as  = renderAttrs attrs
         in start . nam . as . end

  where renderAttrs :: Attributes -> ShowS
        renderAttrs [] = nl
        renderAttrs attrs' = showChar ' ' . ats . nl
          where ats = foldl (.) id $ intersperse (showChar ' ') $ fmap renderAttr attrs'


        renderAttr :: Attribute -> ShowS
        renderAttr (MkAttr (nam, (Value needsEscape val))) = showName nam . showChar '=' . renderAttrVal  (if needsEscape then escape val else val)

        renderAttrVal :: String -> ShowS
        renderAttrVal s = showChar '\"' . showString s . showChar '\"'

        showName (Nothing, s) = showString s
        showName (Just d, s)  = showString d . showChar ':' . showString s

        nl = showChar '\n' . showString (replicate n ' ')

