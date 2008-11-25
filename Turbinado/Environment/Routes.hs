module Turbinado.Environment.Routes (
    addRoutesToEnvironment,
    runRoutes
    ) where
    
import Text.Regex
import Data.Maybe
import Data.Typeable
import Data.Dynamic
import qualified Data.Map as M
import Control.Monad
import qualified Network.HTTP as HTTP
import qualified Network.URI as URI
import Turbinado.Controller.Exception
import {-# SOURCE #-} Turbinado.Environment
import Turbinado.Environment.Logger
import Turbinado.Environment.Request
import Turbinado.Environment.Settings
import qualified Turbinado.Environment.Settings as S

import qualified Config.Routes

type Keys = [String]
data Routes = Routes [(Regex, Keys)]
  deriving (Typeable)

routesKey = "routes"

addRoutesToEnvironment :: EnvironmentFilter
addRoutesToEnvironment = setRoutes $ Routes $ parseRoutes Config.Routes.routes

getRoutes :: Environment -> Routes
getRoutes = getKey routesKey

setRoutes :: Routes -> EnvironmentFilter
setRoutes = setKey routesKey


------------------------------------------------------------------------------
-- Given an Environment
------------------------------------------------------------------------------

runRoutes :: EnvironmentFilter
runRoutes e = do debugM e $ "  Routes.runRoutes : starting"
                 let Routes rs = getRoutes e
                     r         = getRequest e
                     p    = URI.uriPath $ HTTP.rqURI r
                     sets = msum $ map (\(r, k) -> maybe [] (zip k) (matchRegex r p)) rs
                 debugM e $ "  Routes.runRoutes : checking sets"
                 case sets of
                  [] -> throwController $ ParameterLookupFailed $ "No routes matched for " ++ p
                  _  -> do debugM e $ "  Routes.foldl"
                           debugM e $ "  Routes : keys = " ++ (concat $ M.keys $ getSettings e)
                           e' <- foldl (\m (k, v) -> m >>= setSetting k v) (return e) sets
                           debugM e $ "  Routes : keys = " ++ (concat $ M.keys $ getSettings e')
                           debugM e $ "  Routes.addDefaultAction"
                           addDefaultAction e'

addDefaultAction :: EnvironmentFilter
addDefaultAction e = do let s = getSettings e
                        setSettings (M.insertWith (\ a b -> b) "action" (toDyn "Index") s) e

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
