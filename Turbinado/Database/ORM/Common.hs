module Turbinado.Database.ORM.Common where

import Data.Char
import Data.List
import qualified Data.Map as M
import Database.HDBC

import Turbinado.Database.ORM.Types

---------------------------------------------------------------------------
--  Utility functions                                                    --
---------------------------------------------------------------------------
cols :: Columns -> String
cols cs = unwords $  intersperse "," $ map (\s -> "\\\"" ++ s ++ "\\\"") $ M.keys cs

columnToFieldLabel :: (String, (SqlColDesc, ForeignKeyReferences, HasDefault)) -> String
columnToFieldLabel cd@(name, (desc, _, _)) =
  "    " ++ toFunction name  ++ " :: " ++ 
  maybeColumnLabel cd ++
  getHaskellTypeString (colType desc)

maybeColumnLabel :: (String, (SqlColDesc, ForeignKeyReferences, HasDefault)) -> String
maybeColumnLabel (_, (_, _, True)) = "Maybe "  -- Does the column have a default
maybeColumnLabel (_, (desc, _, _)) = if ((colNullable desc) == Just True) then "Maybe " else ""

-- Derived from hdbc-postgresql/Database/PostgreSQL/Statement.hs and hdbc/Database/HDBC/SqlValue.hs
getHaskellTypeString :: SqlTypeId -> String
getHaskellTypeString    SqlCharT = "String"
getHaskellTypeString    SqlVarCharT = "String"
getHaskellTypeString    SqlLongVarCharT = "String"
getHaskellTypeString    SqlWCharT = "String"
getHaskellTypeString    SqlWVarCharT = "String"
getHaskellTypeString    SqlWLongVarCharT = "String"
getHaskellTypeString    SqlDecimalT = "Rational"
getHaskellTypeString    SqlNumericT = "Rational"
getHaskellTypeString    SqlTinyIntT = "Int32"
getHaskellTypeString    SqlSmallIntT ="Int32"
getHaskellTypeString    SqlIntegerT = "Int32"
getHaskellTypeString    SqlBigIntT = "Integer"
getHaskellTypeString    SqlRealT = "Double"
getHaskellTypeString    SqlFloatT = "Double"
getHaskellTypeString    SqlDoubleT = "Double"
getHaskellTypeString    SqlBitT = "Bool"
getHaskellTypeString    SqlDateT = "Day"
getHaskellTypeString    SqlTimestampWithZoneT = "ZonedTime"
getHaskellTypeString    SqlTimestampT = "UTCTime"
getHaskellTypeString    SqlUTCDateTimeT = "UTCTime"
getHaskellTypeString    SqlTimeT = "TimeOfDay"
getHaskellTypeString    SqlUTCTimeT = "TimeOfDay"
getHaskellTypeString    SqlTimeWithZoneT = error "Turbinado ORM Generator: SqlTimeWithZoneT is not supported"
getHaskellTypeString    SqlBinaryT = "B.ByteString"
getHaskellTypeString    SqlVarBinaryT = "B.ByteString"
getHaskellTypeString    SqlLongVarBinaryT = "B.ByteString"
getHaskellTypeString    t = error "Turbinado ORM Generator: Don't know how to translate this SqlTypeId (" ++ show t ++ " to a Haskell Type"


-- | Used for safety.  Lowercases the first letter to 
-- make a valid function.
toFunction [] = error "toFunction passed an empty string"
toFunction (firstLetter:letters) = (Data.Char.toLower firstLetter) : letters


-- | Used for safety.  Uppercases the first letter to 
-- make a valid type.
toType [] = error "toType passed an empty string"
toType (firstLetter:letters) = (Data.Char.toUpper firstLetter) : letters

