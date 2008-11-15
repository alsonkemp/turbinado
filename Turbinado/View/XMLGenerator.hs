module Turbinado.View.XMLGenerator (
        IsAttrValue(..),
        extract,
        module HSX.XMLGenerator,
        HSX.genElement,
        HSX.genEElement
    ) where

import Turbinado.View.Monad
import Turbinado.View.XML hiding (Name)

import HSX.XMLGenerator hiding (XMLGen(..))
import qualified HSX.XMLGenerator as HSX (XMLGen(..))

import Control.Monad.Trans (lift)


---------------------------------------------
-- Instantiating XMLGenerator for the Turbinado monad.

-- | We can use literal XML syntax to generate values of type XML in the Turbinado monad.
instance Monad m => HSX.XMLGen (ViewT' m) where
 type HSX.XML (ViewT' m) = XML
 newtype HSX.Attribute (ViewT' m) = ViewAttr Attribute 
 newtype HSX.Child     (ViewT' m) = ViewChild XML
 xmlToChild = ViewChild
 genElement = element
 genEElement = eElement

instance Monad m => XMLGenerator (ViewT' m)


-- | We must specify an extra rule for Strings even though the previous
-- rule would apply, because the rule for [a] would also apply and
-- would be more specific.
instance Monad m => EmbedAsChild (ViewT' m) String where
 asChild = asChild . pcdata

instance Monad m => EmbedAsChild (ViewT' m) Char where
 asChild = asChild . (:[])


-- | An IO computation returning something that can be represented
-- as a list of XML can be lifted into an analogous Turbinado computation.
instance (EmbedAsChild (ViewT' IO) a) => EmbedAsChild (ViewT' IO) (IO a) where
  asChild ma = lift (lift ma) >>= asChild

-- | Of the base types, () stands out as a type that can only be
-- represented as a (empty) list of XML.
instance Monad m => EmbedAsChild (ViewT' m) () where
  asChild () = return []


-- | Maybe types are handy for cases where you might or might not want
-- to generate XML, such as null values from databases
instance (Monad m, EmbedAsChild (ViewT' m) a) => EmbedAsChild (ViewT' m) (Maybe a) where
  asChild Nothing  = return []
  asChild (Just a) = asChild a

-- This instance should already be there, probably doesn't work due
-- to type families not being fully supported yet.
instance Monad m => EmbedAsChild (ViewT' m) XML where
  asChild = return . return . HSX.xmlToChild

 ---------------
-- IsAttrValue

-- | Instantiate this class to enable values of the given type
-- to appear as XML attribute values.
class Monad m => IsAttrValue m a where
 toAttrValue :: a -> ViewT m AttrValue

-- | An AttrValue is trivial.
instance Monad m => IsAttrValue m AttrValue where
 toAttrValue = return

-- | Strings can be directly represented as values.
instance Monad m => IsAttrValue m String where
 toAttrValue = return . pAttrVal

instance Monad m => IsAttrValue m Int where
 toAttrValue = toAttrValue . show

-- Yeah yeah, map toLower . show, but I'm too
-- lazy to go import Data.Char
instance Monad m => IsAttrValue m Bool where
 toAttrValue True = toAttrValue "true"
 toAttrValue False = toAttrValue "false"

-- | An IO computation returning something that can be represented
-- as an attribute value can be lifted into an analogous Turbinado computation.
instance IsAttrValue IO a => IsAttrValue IO (IO a) where
 toAttrValue ma = lift (lift ma) >>= toAttrValue

-- | An Turbinado computation returning something that can be represented
-- as a list of XML simply needs to turn the produced value into 
-- a list of XML.
instance IsAttrValue m a => IsAttrValue m (ViewT m a) where
 toAttrValue viewa = viewa >>= toAttrValue

-- | Attributes can represent attributes. 
instance Monad m => EmbedAsAttr (ViewT' m) Attribute where
  asAttr = return . return . ViewAttr


-- | Values of the Attr type, constructed with :=, can represent attributes.
instance (IsName n, IsAttrValue m a) => EmbedAsAttr (ViewT' m) (Attr n a) where
  asAttr (n := a) = do
            av <- toAttrValue a
            asAttr $ MkAttr (toName n, av)


-- | ... or of an IO computation.
instance (EmbedAsAttr (ViewT' IO) a) => EmbedAsAttr (ViewT' IO) (IO a) where
  asAttr ma = lift (lift ma) >>= asAttr

-----------------------------------------
-- SetAttr and AppendChild

-- | Set attributes.
instance Monad m => SetAttr (ViewT' m) XML where
 setAll xml hats = do
        attrs <- hats
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n (foldr insert as (map stripAttr attrs)) cs


-- | Append children.
instance Monad m => AppendChild (ViewT' m) XML where
 appAll xml children = do
        chs <- children
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n as (cs ++ (map stripChild chs))

---------------
-- GetAttrValue

-- | Instantiate this class to enable values of the given type
-- to be retrieved through attribute patterns.
class GetAttrValue a where
 fromAttrValue :: AttrValue -> a

-- | An AttrValue is trivial.
instance GetAttrValue AttrValue where
 fromAttrValue = id

-- | Strings can be directly taken from values.
instance GetAttrValue String where
 fromAttrValue (Value _ s) = s
 
-- | Anything that can be read can always fall back on
-- that as a default behavior.
instance (Read a) => GetAttrValue a where
 fromAttrValue = read . fromAttrValue

-- | The common way to present list data in attributes is as
-- a comma separated, unbracketed sequence
instance (GetAttrValue a) => GetAttrValue [a] where
 fromAttrValue v@(Value needsEscape str) = case str of
        [ v1+, (| ',', vs@:(_+) |)+ ] -> 
                map (fromAttrValue . Value needsEscape) (v1:vs)
        _ -> [fromAttrValue v]

-- All that for these two functions
extract :: (GetAttrValue a) => Name -> Attributes -> (Maybe a, Attributes)
extract _ [] = (Nothing, [])
extract name (p@(MkAttr (n, v)):as) 
        | name == n = (Just $ fromAttrValue v, as)
        | otherwise = let (val, attrs) = extract name as
                       in (val, p:attrs)

--------------------------------------------------------------------
-- The base XML generation, corresponding to the use of the literal
-- XML syntax.

--  | Generate an XML element from its components.
element :: (IsName n, 
            EmbedAsChild (ViewT' m) xmls, 
            EmbedAsAttr (ViewT' m) at) 
         => n -> [at] -> xmls -> ViewT m XML
element n attrs xmls = do
        cxml <- asChild xmls
        attribs <- asAttr attrs
        return $ Element (toName n) 
                   (foldr insert eAttrs $ map stripAttr attribs)
                   (flattenCDATA $ map stripChild cxml)
        
  where flattenCDATA :: [XML] -> [XML]
        flattenCDATA cxml = 
                case flP cxml [] of
                 [] -> []
                 [CDATA _ ""] -> []
                 xs -> xs                       
        flP :: [XML] -> [XML] -> [XML]
        flP [] bs = reverse bs
        flP [x] bs = reverse (x:bs)
        flP (x:y:xs) bs = case (x,y) of
                           (CDATA e1 s1, CDATA e2 s2) | e1 == e2 -> flP (CDATA e1 (s1++s2) : xs) bs
                           _ -> flP (y:xs) (x:bs)

stripAttr :: HSX.Attribute (ViewT' m) -> Attribute
stripAttr  (ViewAttr a) = a
stripChild :: HSX.Child (ViewT' m) -> XML
stripChild (ViewChild c) = c

eAttrs :: Attributes
eAttrs = []
insert :: Attribute -> Attributes -> Attributes
insert = (:)

-- | Generate an empty XML element.
eElement :: (IsName n, Monad m, EmbedAsAttr (ViewT' m) at) => n -> [at] -> ViewT m XML
eElement n attrs = element n attrs ([] :: [XML])
