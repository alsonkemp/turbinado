module Turbinado.Environment.Routes (
    addRoutesToEnvironment,
    runRoutes
    ) where

import Control.Concurrent.MVar
import Text.Regex
import Data.Maybe
import Data.Typeable
import Data.Dynamic
import qualified Data.Map as M
import Data.Time
import Control.Monad
import qualified Network.HTTP as HTTP
import qualified Network.URI as URI
import Turbinado.Controller.Monad
import Turbinado.Controller.Exception
import Turbinado.Environment.Types
import Turbinado.Environment.Logger
import Turbinado.Environment.Request
import Turbinado.Environment.Settings
import qualified Turbinado.Environment.Settings as S
import Turbinado.Utility.Data

import qualified Config.Routes

addRoutesToEnvironment :: (HasEnvironment m) => m ()
addRoutesToEnvironment = do e <- getEnvironment
                            let CodeStore mv = fromJust' "Turbinado.Environment.Routes.addRoutesToEnvironment : no CodeStore" $ getCodeStore e
                            cm <- liftIO $ takeMVar mv
                            let cm'  = addStaticControllers Config.Routes.staticControllers cm
                                cm'' = addStaticViews (Config.Routes.staticViews ++ Config.Routes.staticLayouts) cm'
                            liftIO $ putMVar mv cm''
                            setEnvironment $ e {
                              getRoutes = Just $ Routes $ parseRoutes Config.Routes.routes}


------------------------------------------------------------------------------
-- Given an Environment
------------------------------------------------------------------------------

runRoutes :: (HasEnvironment m) => m ()
runRoutes   = do debugM $ "  Routes.runRoutes : starting"
                 e <- getEnvironment
                 let Routes rs = fromJust' "Routes : runRoutes : getRoutes" $ getRoutes e
                     r         = fromJust' "Routes : runRoutes : getRequest" $ getRequest e
                     p    = URI.uriPath $ HTTP.rqURI r
                     sets = filter (not . null) $ map (\(r, k) -> maybe [] (zip k) (matchRegex r p)) rs
                 case sets of
                  [] -> do setSetting "controller" $ last Config.Routes.routes  -- no match, so use the last route
                           addDefaultAction
                  _  -> do mapM (\(k, v) -> setSetting k v) $ head sets
                           addDefaultAction

addDefaultAction :: (HasEnvironment m) => m ()
addDefaultAction   = do e <- getEnvironment
                        let s = fromJust' "Routes : addDefaultAction : getSettings" $ getSettings e
                        setEnvironment $ e {getSettings = Just (M.insertWith (\ a b -> b) "action" (toDyn "Index") s)}

------------------------------------------------------------------------------
-- Generate the Routes from [String]
------------------------------------------------------------------------------

parseRoutes = map (\s -> (routeRegex s, extractKeys s))

extractKeys = map (drop 1) . filter (\l -> (take 1 l) == ":") . ( \s -> concat $ map (splitOn '/') $ splitOn '.' s)
routeRegex = mkRegex . generateRouteRegex

generateRouteKeysRegexString = "([^/^\\.]+)"
generateRouteRegexRegex = mkRegex generateRouteKeysRegexString
generateRouteRegex =  \s -> subRegex generateRouteRegexRegex (escapePeriods s) "([^/^\\.]+)"

escapePeriods []     = []
escapePeriods ('.':cs) = "\\." ++ escapePeriods cs
escapePeriods ( c :cs) = c      : escapePeriods cs

splitOn :: Char -> String -> [String]
splitOn c l = reverse $ worker c l []
     where worker c [] r = r
           worker c (l:ls) [] = if (l == c)
                                  then worker c ls []
                                  else worker c ls [[l]]
           worker c (l:ls) (r:rs) = if (l == c) 
                                      then worker c ls ([]:r:rs)
                                      else worker c ls ((r++[l]):rs)


----------------------------------------------------------------------------
-- Handle static routes
----------------------------------------------------------------------------

--addStaticViews :: [(String, String, View XML)] -> CodeMap -> CodeMap
addStaticViews [] cm = cm
addStaticViews ((p,f,v):vs) cm = let cm' = M.insert (p,f) (CodeLoadView v $ UTCTime (ModifiedJulianDay 1000000) (secondsToDiffTime 0)) cm in
                                 addStaticViews vs cm'

addStaticControllers [] cm = cm
addStaticControllers ((p,f,c):cs) cm = let cm' = M.insert (p,f) (CodeLoadController c $ UTCTime (ModifiedJulianDay 1000000) (secondsToDiffTime 0)) cm in
                                       addStaticControllers cs cm'

