module Turbinado.View (
        getEnvironment,
        setEnvironment,
        evalView,
        defaultContentType,
        modifyEnvironment,
        -- limited export from Turbinado.View.Monad
        View, ViewT, ViewT',
        runView, runViewT,
        get, put,
        -- * Functions
        doIO, catch,

        -- Module Exports
        module Turbinado.View.HTML,
        module Turbinado.View.XML,
        module Turbinado.View.XML.PCDATA,
        module Turbinado.View.XMLGenerator,
        module Turbinado.Environment,
        module Turbinado.Environment.CodeStore,
        module Turbinado.Environment.Request,
        module Turbinado.Environment.Response,
        module Turbinado.Environment.Settings,
        ) where

import Control.Exception (catchDyn)
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans (MonadIO(..))
import Data.List
import Data.Maybe
import qualified Network.HTTP as HTTP
import qualified Network.URI  as URI
import Prelude hiding (catch)
import System.FilePath

import Turbinado.Environment
import Turbinado.Environment.Request
import Turbinado.Environment.Response
import Turbinado.Environment.Settings
import Turbinado.View.Exception
import Turbinado.View.HTML
import Turbinado.View.Monad
import Turbinado.View.XML hiding (Name)
import Turbinado.View.XML.PCDATA
import Turbinado.View.XMLGenerator
import Turbinado.Environment.CodeStore
import Turbinado.Utility.General


evalView :: View XML -> EnvironmentFilter
evalView p e =
           do (x, e') <- runView p e
              case (HTTP.rspCode $ getResponse e') of
                (0,0,0) -> setResponse ((getResponse e) {HTTP.rspCode = (2,0,0), HTTP.rspBody = renderAsHTML x}) e'
                _       -> return e'

defaultContentType :: String
defaultContentType = "text/html; charset=ISO-8859-1"

--
-- * Environment functions
--

getEnvironment :: View Environment
getEnvironment = lift get

setEnvironment :: Environment -> View ()
setEnvironment e = lift $ put e

modifyEnvironment :: (Environment -> Environment) -> View ()
modifyEnvironment =  lift . modify

--
-- * Header functions
--

--
-- * Cookie functions
--

{-
-- | Get the value of a cookie.
getCookie :: String           -- ^ The name of the cookie.
          -> View (Maybe String) -- ^ 'Nothing' if the cookie does not exist.
getCookie name = getRequest >>= \r -> return $ Cookie.findCookie name 
                        (fromMaybe "" $ HTTP.lookupHeader HTTP.HdrCookie (HTTP.rqHeaders $ httpRequest r))

-- | Same as 'getCookie', but tries to read the value to the desired type.
readCookie :: (Read a) =>
              String       -- ^ The name of the cookie.
           -> View (Maybe a)  -- ^ 'Nothing' if the cookie does not exist
                           --   or if the value could not be interpreted
                           --   at the desired type.
readCookie = liftM (>>= maybeRead) . getCookie

-- | Set a cookie.
setCookie :: Cookie.Cookie -> View HTTP.Response
setCookie c = getResponse >>= return . HTTP.replaceHeader HTTP.HdrSetCookie (Cookie.showCookie c)
                                                           
-- | Delete a cookie from the client
deleteCookie :: Cookie.Cookie -> View HTTP.Response
deleteCookie = setCookie . Cookie.deleteCookie
-}


