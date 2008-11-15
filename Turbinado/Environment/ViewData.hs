module Turbinado.Environment.ViewData (
        addViewDataToEnvironment,
        getViewData,
        setViewData
        )where

import qualified Data.Map as M
import Control.Monad
import Data.Maybe
import Turbinado.Environment

type ViewData = Map String Dynamic


viewDataKey = "viewdata"

addViewDataToEnvironment :: EnvironmentFilter
addViewDataToEnvironment  = setViewData (empty :: ViewData)

getViewData :: Environment -> ViewData
getViewData = getKey viewDataKey

setViewData :: ViewData -> EnvironmentFilter
setViewData vd = setKey viewDataKey vd

getViewDataValue :: (Typeable a) => String -> Environment -> a
getViewDataValue s e = lookup (getViewData e) s
