module Turbinado.Database.ORM.Adapters.Common where

import qualified Data.Char
import Control.Monad
import Data.Dynamic
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Database.HDBC
import System.Directory
import System.FilePath

import Turbinado.Database.ORM.Common
import Turbinado.Database.ORM.Types


---------------------------------------------------------------------------
--  File templates                                                       --
---------------------------------------------------------------------------

generateType ::   TableName ->
                  TypeName ->
                  PrimaryKey -> 
                  Tables -> 
                  Columns -> 
                  String
generateType t typeName pk ts cs = 
  unlines $
  ["{- DO NOT EDIT THIS FILE"
  ,"   THIS FILE IS AUTOMAGICALLY GENERATED AND YOUR CHANGES WILL BE EATEN BY THE GENERATOR OVERLORD"
  ,""
  ,"   All changes should go into the Model file (e.g. App/Models/ExampleModel.hs)"
  ,"-}"
  ,""
  ,"module App.Models.Bases." ++ typeName ++ "Type where"
  , ""
  , "import App.Models.Bases.Common"
  , "import Data.Maybe"
  , "import Data.Time"
  , "import Data.Typeable"
  , ""
  ] ++
  ["-- The data type for this model"] ++
  [ "data " ++ typeName ++ " = " ++ typeName ++ " {"
  ] ++
  [intercalate ",\n" (map columnToFieldLabel (M.toList cs))] ++
  [ "    } deriving (Show, Typeable)"
  , ""
  , "instance DatabaseModel " ++ typeName ++ " where"
  , "    tableName _ = \"" ++ t ++ "\""
  , ""
  ]

generateFunctions ::  TableName ->
                      TypeName ->
                      PrimaryKey -> 
                      Tables -> 
                      Columns -> 
                      String
generateFunctions t typeName pk ts cs = 
  unlines $
  ["{- DO NOT EDIT THIS FILE"
  ,"   THIS FILE IS AUTOMAGICALLY GENERATED AND YOUR CHANGES WILL BE EATEN BY THE GENERATOR OVERLORD"
  ,""
  ,"   All changes should go into the Model file (e.g. App/Models/ExampleModel.hs)"
  ,"-}"
  ,""
  ,"module App.Models.Bases." ++ typeName ++ "Functions where"
  , ""
  , "import App.Models.Bases.Common"
  , "import qualified Database.HDBC as HDBC"
  , "import Data.Maybe"
  , "import Data.Time"
  , ""
  , " -- My type"
  , "import App.Models.Bases." ++ typeName ++ "Type"
  , ""
  , "import Turbinado.Environment.Types"
  , "import Turbinado.Environment.Database"
  , ""
  ] ++
  [""] ++
  generateHasFindByPrimaryKey t cs typeName pk ++
  [""] ++
  generateIsModel t cs typeName
  ++
  [""
  ,"deleteWhere :: (HasEnvironment m) => SelectString -> SelectParams -> m Integer"
  ,"deleteWhere ss sp = do "
  ,"        conn <- getEnvironment >>= (return . fromJust . getDatabase )"
  ,"        catchDBErrors conn $ do"
  ,"          res <- liftIO $ HDBC.handleSqlError $ HDBC.run conn (\"DELETE FROM \\\"" ++ t ++ "\\\" WHERE (\" ++ ss ++ \") \")  sp"
  ,"          return res"
  ]
generateRelations ::  TableName ->
                      TypeName ->
                      PrimaryKey -> 
                      Tables -> 
                      Columns -> 
                      String
generateRelations t typeName pk ts cs = 
  unlines $
  ["{- DO NOT EDIT THIS FILE"
  ,"   THIS FILE IS AUTOMAGICALLY GENERATED AND YOUR CHANGES WILL BE EATEN BY THE GENERATOR OVERLORD"
  ,""
  ,"   All changes should go into the Model file (e.g. App/Models/ExampleModel.hs)"
  ,"-}"
  ,""
  ,"module App.Models.Bases." ++ typeName ++ "Relations where"
  , ""
  , "import App.Models.Bases.Common"
  , "import qualified Database.HDBC as HDBC"
  , "import Data.Maybe"
  , "import Data.Time"
  , ""
  , " -- Model imports"
  , "import App.Models.Bases." ++ typeName ++ "Type"
  , unlines $ generateChildModelImports cs 
  , unlines $ generateParentModelImports t ts
  , ""
  , "import Turbinado.Environment.Types"
  , "import Turbinado.Environment.Database"
  , ""
  ] ++
  [""] ++
  [""] ++
  generateHasChildren t cs typeName ++
  [""] ++
  [""] ++
  generateHasParents t ts

generateChildModelImports cs = 
    map (\ctn -> "import qualified App.Models.Bases." ++ toType ctn ++ "Type as " ++ toType ctn ++ "Type\nimport qualified App.Models.Bases." ++ toType ctn ++ "Functions as " ++ toType ctn ++ "Functions") $ 
      nub $
        map fst $ concat $
          map (\(_, fks, _) -> fks) $ M.elems cs

generateParentModelImports t ts = 
    map (\ptn -> "import qualified App.Models.Bases." ++ toType ptn ++ "Type as " ++ toType ptn ++ "Type\nimport qualified App.Models.Bases." ++ toType ptn ++ "Functions as " ++ toType ptn ++ "Functions") $ 
      nub $ filter (not . null) $ 
        map parentFilter $ M.assocs ts
    where parentFilter (ptn, (cs, _)) = 
             case (filter (\(tn, _) -> t == tn) $ concat $ map (\(_, fks, _) -> fks) $ M.elems cs) of
               [] -> []
               _  -> ptn

generateModelFile typeName =
  unlines $
  ["module App.Models." ++ typeName
  ,"  ( module App.Models." ++ typeName
  ,"  , module App.Models.Bases." ++ typeName ++ "Type"
  ,"  , module App.Models.Bases." ++ typeName ++ "Functions"
  ,"  , module App.Models.Bases." ++ typeName ++ "Relations"
  ,"  , module App.Models.Bases.Common"
  ,"  ) where"
  ,"import App.Models.Bases." ++ typeName ++ "Type"
  ,"import App.Models.Bases." ++ typeName ++ "Functions"
  ,"import App.Models.Bases." ++ typeName ++ "Relations"
  ,"import App.Models.Bases.Common"
  ]

generateCommon:: String
generateCommon = unlines $
  ["{- DO NOT EDIT THIS FILE"
  ,"   THIS FILE IS AUTOMAGICALLY GENERATED AND YOUR CHANGES WILL BE EATEN BY THE GENERATOR OVERLORD -}"
  ,""
  ,"module App.Models.Bases.Common("
  ,"  module App.Models.Bases.Common,"
  ,"  module Control.OldException,"
  ,"  module Control.Monad.Trans,"
  ,"  module Data.Int"
  ,"  ) where"
  ,""
  ,"import Control.Monad.Trans"
  ,"import Control.OldException"
  ,"import Database.HDBC"
  ,"import Data.Int"
  ,""
  ,"import Turbinado.Environment.Types"
  ,""
  ,"-- Using phantom types here "
  ,"class DatabaseModel m where"
  ,"  tableName :: m -> String"
  ,""
  ,"type SelectString = String"
  ,"type SelectParams = [SqlValue]"
  ,"type OrderByParams  = String"
  ,""
  ,"-- Exception handling"
  ,""
  ,"catchDBErrors :: (HasEnvironment m) => ConnWrapper -> IO a -> m a"
  ,"catchDBErrors c fdb = liftIO $ catchSql fdb (\\e-> (handleSqlError $ rollback c) >>"
  ,"                                                   (throwDyn $ e))"
  ,""
  ,"class (DatabaseModel model) =>"
  ,"        IsModel model where"
  ,"        insert    :: (HasEnvironment m) => model -> Bool -> m (Maybe Integer)"
  ,"        findAll   :: (HasEnvironment m) => m [model]"
  ,"        findAllWhere :: (HasEnvironment m) => SelectString -> SelectParams -> m [model]"
  ,"        findAllOrderBy :: (HasEnvironment m) => OrderByParams -> m [model]"
  ,"        findAllWhereOrderBy :: (HasEnvironment m) => SelectString -> SelectParams -> OrderByParams -> m [model]"
  ,"        findOneWhere :: (HasEnvironment m) => SelectString -> SelectParams -> m model"
  ,"        findOneOrderBy :: (HasEnvironment m) => OrderByParams -> m model"
  ,"        findOneWhereOrderBy :: (HasEnvironment m) => SelectString -> SelectParams -> OrderByParams -> m model"
  ,""
  ,"class (DatabaseModel model) =>"
  ,"        HasFindByPrimaryKey model primaryKey | model -> primaryKey where"
  ,"    find   :: (HasEnvironment m) => primaryKey -> m model"
  ,"    delete :: (HasEnvironment m) => primaryKey -> m ()"
  ,"    update :: (HasEnvironment m) => model      -> m ()"
  ,""
  ]

---------------------------------------------------------------------------
--  Generator templates                                                  --
---------------------------------------------------------------------------

generateIsModel :: TableName -> Columns -> TypeName -> [String]
generateIsModel t cs typeName =
    ["instance IsModel " ++ typeName ++ " where"
    ,"    insert m returnId = do"
    ,"        conn <- getEnvironment >>= (return . fromJust . getDatabase )"
    ,"        catchDBErrors conn $ do"
    ,"          res  <- HDBC.run conn (\" INSERT INTO \\\"" ++ t ++ "\\\" (" ++ (cols cs) ++") VALUES (" ++ (intercalate "," $ map generateQs (M.assocs cs) ) ++ ")\")  ( " ++ (intercalate " ++ " $ filter (not . null) $ map generateArgs (M.assocs cs) ) ++ ")"
    ,"          case res of"
    ,"            0 -> (HDBC.handleSqlError $ HDBC.rollback conn) >>"
    ,"                 (throwDyn $ HDBC.SqlError"
    ,"                             {HDBC.seState = \"\","
    ,"                              HDBC.seNativeError = (-1),"
    ,"                              HDBC.seErrorMsg = \"Rolling back.  No record inserted :" ++ t ++ " : \" ++ (show m)"
    ,"                             })"
    ,"            1 -> HDBC.handleSqlError $ HDBC.commit conn >>"
    ,"                 if returnId"
    ,"                   then do i <- HDBC.catchSql (HDBC.handleSqlError $ HDBC.quickQuery' conn \"SELECT lastval()\" []) (\\_ -> HDBC.commit conn >> (return $ [[HDBC.toSql (0 :: Integer)]]) ) "
    ,"                           return $ HDBC.fromSql $ head $ head i"
    ,"                 else return Nothing"
    ,"    findAll = do"
    ,"        conn <- getEnvironment >>= (return . fromJust . getDatabase )"
    ,"        res <- liftIO $ HDBC.handleSqlError $ HDBC.quickQuery' conn \"SELECT " ++ cols cs ++ " FROM \\\"" ++ t ++ "\\\" \" []"
    ,"        return $ map (\\r -> " ++ generateConstructor cs typeName ++ ") res"
    ,"    findAllWhere ss sp = do"
    ,"        conn <- getEnvironment >>= (return . fromJust . getDatabase )"
    ,"        res <- liftIO $ HDBC.handleSqlError $ HDBC.quickQuery' conn (\"SELECT " ++ cols cs ++ " FROM \\\"" ++ t++ "\\\" WHERE (\" ++ ss ++ \") \")  sp"
    ,"        return $ map (\\r -> " ++ generateConstructor cs typeName ++ ") res"
    ,"    findAllOrderBy op = do"
    ,"        conn <- getEnvironment >>= (return . fromJust . getDatabase )"
    ,"        res <- liftIO $ HDBC.handleSqlError $ HDBC.quickQuery' conn (\"SELECT " ++ cols cs ++ " FROM \\\"" ++ t++ "\\\" ORDER BY \" ++ op) []"
    ,"        return $ map (\\r -> " ++ generateConstructor cs typeName ++ ") res"
    ,"    findAllWhereOrderBy ss sp op = do"
    ,"        conn <- getEnvironment >>= (return . fromJust . getDatabase )"
    ,"        res <- liftIO $ HDBC.handleSqlError $ HDBC.quickQuery' conn (\"SELECT " ++ cols cs ++ " FROM \\\"" ++ t++ "\\\" WHERE (\" ++ ss ++ \") ORDER BY \" ++ op) sp"
    ,"        return $ map (\\r -> " ++ generateConstructor cs typeName ++ ") res"
    ,"    findOneWhere ss sp = do"
    ,"        conn <- getEnvironment >>= (return . fromJust . getDatabase )"
    ,"        res <- liftIO $ HDBC.handleSqlError $ HDBC.quickQuery' conn (\"SELECT " ++ cols cs ++ " FROM \\\"" ++ t++ "\\\" WHERE (\" ++ ss ++ \") LIMIT 1\") sp"
    ,"        return $ (\\r -> " ++ generateConstructor cs typeName ++ ") (head res)"
    ,"    findOneOrderBy op = do"
    ,"        conn <- getEnvironment >>= (return . fromJust . getDatabase )"
    ,"        res <- liftIO $ HDBC.handleSqlError $ HDBC.quickQuery' conn (\"SELECT " ++ cols cs ++ " FROM \\\"" ++ t++ "\\\" ORDER BY \" ++ op ++ \" LIMIT 1\")  []"
    ,"        return $ (\\r -> " ++ generateConstructor cs typeName ++ ") (head res)"
    ,"    findOneWhereOrderBy ss sp op = do"
    ,"        conn <- getEnvironment >>= (return . fromJust . getDatabase )"
    ,"        res <- liftIO $ HDBC.handleSqlError $ HDBC.quickQuery' conn (\"SELECT " ++ cols cs ++ " FROM \\\"" ++ t++ "\\\" WHERE (\" ++ ss ++ \") ORDER BY \" ++ op ++\" LIMIT 1\")  sp"
    ,"        return $ (\\r -> " ++ generateConstructor cs typeName ++ ") (head res)"
     ]
       where generateQs   :: (String, (SqlColDesc, ForeignKeyReferences, HasDefault)) -> String
             generateQs (c, (desc, _, False)) = if ((colNullable desc) == Just True) then ("\" ++ (case (" ++ toFunction c ++ " m) of Nothing -> \"DEFAULT\"; Just x -> \"?\") ++ \"") else "?"
             generateQs (c, (_, _, True)) = "\" ++ (case (" ++ toFunction c ++ " m) of Nothing -> \"DEFAULT\"; Just x -> \"?\") ++ \"" 
             generateArgs :: (String, (SqlColDesc, ForeignKeyReferences, HasDefault)) -> String
             generateArgs (c, (desc, _, False)) = if ((colNullable desc) == Just True) then ("(case (" ++ toFunction c ++ " m) of Nothing -> []; Just x -> [HDBC.toSql x])") else ("[HDBC.toSql $ " ++ toFunction c ++ " m]")
             generateArgs (c, (_, _, True)) = "(case (" ++ toFunction c ++ " m) of Nothing -> []; Just x -> [HDBC.toSql x])" 

generateHasFindByPrimaryKey :: TableName -> Columns -> TypeName -> PrimaryKey -> [String]
generateHasFindByPrimaryKey t cs typeName pk =
  case (length  pk) of
    0 -> [""]
    _ -> ["instance HasFindByPrimaryKey " ++ typeName ++ " " ++ " (" ++ unwords (intersperse "," (map (\c -> getHaskellTypeString $ colType $ (\(c',_,_) -> c') $ fromJust $ M.lookup c cs) pk)) ++ ") " ++ " where"
         ,"    find pk@(" ++ (concat $ intersperse ", " $ map (\i -> "pk"++(show i)) [1..(length pk)]) ++ ") = do"
         ,"        conn <- getEnvironment >>= (return . fromJust . getDatabase )"
         ,"        res <- liftIO $ HDBC.handleSqlError $ HDBC.quickQuery' conn (\"SELECT " ++ cols cs ++ " FROM \\\"" ++ t ++ "\\\" WHERE (" ++ (generatePrimaryKeyWhere pk) ++ ")\") [" ++ (unwords $ intersperse "," $ map (\(c,i) -> "HDBC.toSql pk" ++ (show i)) (zip pk [1..])) ++ "]"
         ,"        case res of"
         ,"          [] -> throwDyn $ HDBC.SqlError"
         ,"                           {HDBC.seState = \"\","
         ,"                            HDBC.seNativeError = (-1),"
         ,"                            HDBC.seErrorMsg = \"No record found when finding by Primary Key:" ++ t ++ " : \" ++ (show pk)"
         ,"                           }"
         ,"          r:[] -> return $ " ++ (generateConstructor cs typeName)
         ,"          _ -> throwDyn $ HDBC.SqlError"
         ,"                           {HDBC.seState = \"\","
         ,"                            HDBC.seNativeError = (-1),"
         ,"                            HDBC.seErrorMsg = \"Too many records found when finding by Primary Key:" ++ t ++ " : \" ++ (show pk)"
         ,"                           }"
         ,""
         ,"    delete pk@(" ++ (concat $ intersperse ", " $ map (\i -> "pk"++(show i)) [1..(length pk)]) ++ ") = do"
         ,"        conn <- getEnvironment >>= (return . fromJust . getDatabase )"
         ,"        catchDBErrors conn $ do"
         ,"          res <- HDBC.run conn (\"DELETE FROM \\\"" ++ t ++ "\\\" WHERE (" ++ (generatePrimaryKeyWhere pk) ++ ")\") [" ++ (unwords $ intersperse "," $ map (\(c,i) -> "HDBC.toSql pk" ++ (show i)) (zip pk [1..])) ++ "]"
         ,"          case res of"
         ,"            0 -> (HDBC.handleSqlError $ HDBC.rollback conn) >>"
         ,"                 (throwDyn $ HDBC.SqlError"
         ,"                             {HDBC.seState = \"\","
         ,"                              HDBC.seNativeError = (-1),"
         ,"                              HDBC.seErrorMsg = \"Rolling back.  No record found when deleting by Primary Key:" ++ t ++ " : \" ++ (show pk)"
         ,"                             })"
         ,"            1 -> (HDBC.handleSqlError $ HDBC.commit conn) >> return ()"
         ,"            _ -> (HDBC.handleSqlError $ HDBC.rollback conn) >>"
         ,"                 (throwDyn $ HDBC.SqlError"
         ,"                             {HDBC.seState = \"\","
         ,"                              HDBC.seNativeError = (-1),"
         ,"                              HDBC.seErrorMsg = \"Rolling back.  Too many records deleted when deleting by Primary Key:" ++ t ++ " : \" ++ (show pk)"
         ,"                             })"
         ,""
         ,"    update m = do"
         ,"        conn <- getEnvironment >>= (return . fromJust . getDatabase )"
         ,"        catchDBErrors conn $ do"
         ,"          res <- HDBC.run conn \"UPDATE \\\"" ++ t ++ "\\\" SET (" ++ (cols cs) ++ ") = (" ++ (intercalate "," $ (take (M.size cs) (repeat "?"))) ++ ") WHERE (" ++ (generatePrimaryKeyWhere pk)  ++")\""
         ,"                    [" ++ (unwords $ intersperse "," $ map (\c -> "HDBC.toSql $ " ++ toFunction c ++ " m") (M.keys cs) ) ++ ", " ++ (unwords $ intersperse "," $ map (\c -> "HDBC.toSql $ " ++ toFunction c ++ " m") pk ) ++ "]"
         ,"          HDBC.handleSqlError $ HDBC.commit conn"
         ,"          return ()"
         ]

generateHasChildren :: TableName -> Columns -> TypeName -> [String]
generateHasChildren t cs typeName = map (\(cn, cd) -> generateHasChildren_t t cn cd typeName) $ M.assocs cs

generateHasChildren_t :: TableName -> ColumnName -> ColumnDesc -> TypeName -> String
generateHasChildren_t t cn (_, fks, _) typeName = unlines $ map (\(fkt, fkc) -> generateHasChildren_t_k t cn fkt fkc typeName) fks

generateHasChildren_t_k :: TableName -> ColumnName -> TableName -> ColumnName -> TypeName -> String
generateHasChildren_t_k t cn fkt fkc typeName = 
  unlines $
    ["findAllChild" ++ toType fkt  ++ " :: (HasEnvironment m) => " ++ toType t ++ " -> m [" ++ toType fkt ++ "Type." ++ toType fkt ++ "]"
    ,"findAllChild" ++ toType fkt ++ " p = findAllWhere \"" ++ fkc ++ " = ?\" [HDBC.toSql $ " ++ toFunction cn ++ " p]"
    ]


generateHasParents :: TableName -> Tables -> [String]
generateHasParents ctn ts =
    map (\(tname, cname, ptname, pcname) -> generateHasParent_t tname cname ptname pcname) $ 
      nub $ concat $ 
        map parentFilter $ M.assocs ts
    where parentFilter (tn, (cs', _)) = filter (\(_, _, tn', _) -> ctn == tn') $ concat $ map (\(cn, (_, fks, _)) -> map (\(ptn, pcn) -> (tn, cn, ptn, pcn)) fks) $ M.assocs cs'



generateHasParent_t :: TableName -> ColumnName -> TableName -> ColumnName -> String
generateHasParent_t ptn pcn ctn ccn = 
  unlines $
    ["parent" ++ toType ptn ++ " :: (HasEnvironment m) => " ++ toType ctn ++ " -> m " ++ toType ptn ++ "Type." ++ toType ptn
    ,"parent" ++ toType ptn ++ " self = findOneWhere \"" ++ pcn ++ " = ?\" [HDBC.toSql $ " ++ toFunction ccn ++ " self]"
    ]


{-----------------------------------------------------------------------}

generatePrimaryKeyWhere pk =
  unwords $
    intersperse " AND " $
      map (\(c,i) -> "\\\"" ++ c ++ "\\\" = ? ") (zip pk [1..])


generateConstructor cs typeName =
  typeName ++ " " ++ (unwords $
  map (\i -> "(HDBC.fromSql (r !! " ++ (show i) ++ "))") [0..((M.size cs) - 1)])
