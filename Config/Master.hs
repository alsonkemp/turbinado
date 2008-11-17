module Config.Master (
        module Config.Master,
        module Config.App,
        Turbinado.Server.Handlers.SessionHandlers.Simple.getSessionHandler
        ) where

import Turbinado.Server.Handlers.SessionHandlers.Simple
import Config.App

----------------------------------------------------------------
-- Arguments to the make system used in the Dynamic Loader
----------------------------------------------------------------

compileArgs =
        [ "-fglasgow-exts"
        , "-fallow-overlapping-instances"
        , "-fallow-undecidable-instances"
        , "-F", "-pgmFtrhsx"
        , "-fno-warn-overlapping-patterns" 
        , "-odir " ++ compiledDir
        , "-hidir " ++ compiledDir
        ] ++ (map ("-i"++) searchDirs)

mUserPkgConf = [""]

----------------------------------------------------------------
-- Paths
----------------------------------------------------------------

viewDir        = "App/Views"
viewStub       = "Turbinado/Stubs/View.hs"
layoutDir      = "App/Layouts"
layoutStub     = "Turbinado/Stubs/Layout.hs"
controllerDir  = "App/Controllers"
controllerStub = "Turbinado/Stubs/Controller.hs"

configDir = "Config"
searchDirs = [viewDir, layoutDir, controllerDir, rootDir, configDir, compiledDir]

staticDirs = ["static", "tmp/cache"]
compiledDir = "tmp/compiled"

rootDir = "./"
