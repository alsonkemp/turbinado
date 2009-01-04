{-# OPTIONS -fglasgow-exts #-}
--
-- Copyright (C) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- 
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
-- 
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
-- 
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
-- USA
-- 

-- | An interface to the GHC runtime's dynamic linker, providing runtime
-- loading and linking of Haskell object files, commonly known as
-- /plugins/.

module System.Plugins.Load (

      -- * The @LoadStatus@ type
      LoadStatus(..)

      -- * High-level interface
      , load
      , load_
      , dynload
      , pdynload 
      , pdynload_
      , unload
      , unloadAll
      , reload
      , Module(..)

      -- * Low-level interface
      , initLinker      -- start it up
      , loadModule      -- load a vanilla .o
      , loadFunction    -- retrieve a function from an object
      , loadFunction_   -- retrieve a function from an object
      , loadPackageFunction
      , loadPackage     -- load a ghc library and its cbits
      , unloadPackage   -- unload a ghc library and its cbits
      , loadPackageWith -- load a pkg using the package.conf provided
      , loadShared      -- load a .so object file
      , resolveObjs     -- and resolve symbols

      , loadRawObject   -- load a bare .o. no dep chasing, no .hi file reading

      , Symbol

      , getImports

  ) where

#include "../../../config.h"

import System.Plugins.Make             ( build )
import System.Plugins.Env
import System.Plugins.Utils
import System.Plugins.Consts           ( sysPkgSuffix, hiSuf, prefixUnderscore )
import System.Plugins.LoadTypes

-- import Language.Hi.Parser
import BinIface
import HscTypes
import Module (moduleName, moduleNameString)
import PackageConfig (packageIdString)
import HscMain (newHscEnv)
import TcRnMonad (initTcRnIf)

import Data.Dynamic          ( fromDynamic, Dynamic )
import Data.Typeable         ( Typeable )

import Data.List                ( isSuffixOf, nub, nubBy )
import Control.Monad            ( when, filterM, liftM )
import System.Directory         ( doesFileExist, removeFile )
import Foreign.C.String         ( CString, withCString, peekCString )

import GHC.Ptr                  ( Ptr(..), nullPtr )
import GHC.Exts                 ( addrToHValue# )
import GHC.Prim                 ( unsafeCoerce# )

#if DEBUG
import System.IO                ( hFlush, stdout )
#endif
import System.IO                ( hClose )

ifaceModuleName = moduleNameString . moduleName . mi_module

readBinIface' :: FilePath -> IO ModIface
readBinIface' hi_path = do
    -- kludgy as hell
    e <- newHscEnv undefined
    initTcRnIf 'r' e undefined undefined (readBinIface hi_path)

-- TODO need a loadPackage p package.conf :: IO () primitive

--
-- | The @LoadStatus@ type encodes the return status of functions that
-- perform dynamic loading in a type isomorphic to 'Either'. Failure
-- returns a list of error strings, success returns a reference to a
-- loaded module, and the Haskell value corresponding to the symbol that
-- was indexed.
--
data LoadStatus a
        = LoadSuccess Module a
        | LoadFailure Errors

--
-- | 'load' is the basic interface to the dynamic loader. A call to
-- 'load' imports a single object file into the caller's address space,
-- returning the value associated with the symbol requested. Libraries
-- and modules that the requested module depends upon are loaded and
-- linked in turn.
--
-- The first argument is the path to the object file to load, the second
-- argument is a list of directories to search for dependent modules.
-- The third argument is a list of paths to user-defined, but
-- unregistered, /package.conf/ files. The 'Symbol' argument is the
-- symbol name of the value you with to retrieve.
--
-- The value returned must be given an explicit type signature, or
-- provided with appropriate type constraints such that Haskell compiler
-- can determine the expected type returned by 'load', as the return
-- type is notionally polymorphic.
-- 
-- Example:
--
-- > do mv <- load "Plugin.o" ["api"] [] "resource"
-- >    case mv of
-- >        LoadFailure msg -> print msg
-- >        LoadSuccess _ v -> return v
--
load :: FilePath                -- ^ object file
     -> [FilePath]              -- ^ any include paths
     -> [PackageConf]           -- ^ list of package.conf paths
     -> Symbol                  -- ^ symbol to find
     -> IO (LoadStatus a)

load obj incpaths pkgconfs sym = do
    initLinker

    -- load extra package information
    mapM_ addPkgConf pkgconfs
    (hif,moduleDeps) <- loadDepends obj incpaths

    -- why is this the package name?
#if DEBUG
    putStr (' ':(decode $ ifaceModuleName hif)) >> hFlush stdout
#endif

    m' <- loadObject obj . Object . ifaceModuleName $ hif
    let m = m' { iface = hif }
    resolveObjs (mapM_ unloadAll (m:moduleDeps))

#if DEBUG
    putStrLn " ... done" >> hFlush stdout
#endif
    addModuleDeps m' moduleDeps
    v <- loadFunction m sym
    return $ case v of 
        Nothing -> LoadFailure ["load: couldn't find symbol <<"++sym++">>"]
        Just a  -> LoadSuccess m a

--
-- | Like load, but doesn't want a package.conf arg (they are rarely used)
--
load_ :: FilePath -> [FilePath] -> Symbol -> IO (LoadStatus a)
load_ o i s = load o i [] s

--
-- A work-around for Dynamics. The keys used to compare two TypeReps are
-- somehow not equal for the same type in hs-plugin's loaded objects.
-- Solution: implement our own dynamics...
--
-- The problem with dynload is that it requires the plugin to export
-- a value that is a Dynamic (in our case a (TypeRep,a) pair). If this
-- is not the case, we core dump. Use pdynload if you don't trust the
-- user to supply you with a Dynamic
--
dynload :: Typeable a    
        => FilePath  
        -> [FilePath]
        -> [PackageConf]
        -> Symbol
        -> IO (LoadStatus a)

dynload obj incpaths pkgconfs sym = do
    s <- load obj incpaths pkgconfs sym
    case s of e@(LoadFailure _)   -> return e
              LoadSuccess m dyn_v -> return $
                    case fromDynamic (unsafeCoerce# dyn_v :: Dynamic) of
                        Just v' -> LoadSuccess m v'
                        Nothing -> LoadFailure ["Mismatched types in interface"]

------------------------------------------------------------------------
--
-- The super-replacement for dynload
--
-- Use GHC at runtime so we get staged type inference, providing full
-- power dynamics, *on module interfaces only*. This is quite suitable
-- for plugins, of coures :)
--
-- TODO where does the .hc file go in the call to build() ?
--

pdynload :: FilePath                    -- ^ object to load
         -> [FilePath]                  -- ^ include paths
         -> [PackageConf]               -- ^ package confs
         -> Type                        -- ^ API type
         -> Symbol                      -- ^ symbol
         -> IO (LoadStatus a)

pdynload object incpaths pkgconfs ty sym = do 
#if DEBUG
        putStr "Checking types ... " >> hFlush stdout
#endif
        errors <- unify object incpaths [] ty sym
#if DEBUG
        putStrLn "done"
#endif
        if null errors 
                then load object incpaths pkgconfs sym
                else return $ LoadFailure errors

--
-- | Like pdynload, but you can specify extra arguments to the
-- typechecker.
--
pdynload_ :: FilePath       -- ^ object to load
          -> [FilePath]     -- ^ include paths for loading
          -> [PackageConf]  -- ^ any extra package.conf files
          -> [Arg]          -- ^ extra arguments to ghc, when typechecking
          -> Type           -- ^ expected type
          -> Symbol         -- ^ symbol to load
          -> IO (LoadStatus a)

pdynload_ object incpaths pkgconfs args ty sym = do
#if DEBUG
        putStr "Checking types ... " >> hFlush stdout
#endif
        errors <- unify object incpaths args ty sym
#if DEBUG
        putStrLn "done"
#endif
        if null errors 
                then load object incpaths pkgconfs sym
                else return $ LoadFailure errors

------------------------------------------------------------------------
-- run the typechecker over the constraint file
--
-- Problem: if the user depends on a non-auto package to build the
-- module, then that package will not be in scope when we try to build
-- the module, when performing `unify'. Normally make() will handle this
-- (as it takes extra ghc args). pdynload ignores these, atm -- but it
-- shouldn't. Consider a pdynload() that accepts extra -package flags?
--
-- Also, pdynload() should accept extra in-scope modules.
-- Maybe other stuff we want to hack in here.
--
unify obj incs args ty sym = do
        (tmpf,hdl)   <- mkTemp
        (tmpf1,hdl1) <- mkTemp  -- and send .hi file here.
        hClose hdl1

        let nm  = mkModid (basename tmpf) 
            src = mkTest nm (hierize' . mkModid . hierize $ obj)
                                (fst $ break (=='.') ty) ty sym
            is  = map ("-i"++) incs             -- api
            i   = "-i" ++ dirname obj           -- plugin

        hWrite hdl src

        e <- build tmpf tmpf1 (i:is++args++["-fno-code","-ohi "++tmpf1])
        mapM_ removeFile [tmpf,tmpf1]
        return e

        where
            -- fix up hierarchical names
            hierize []       = []
            hierize ('/':cs) = '\\' : hierize cs
            hierize (c:cs)   = c    : hierize cs

            hierize'[]        = []
            hierize' ('\\':cs) = '.' : hierize' cs
            hierize' (c:cs)   = c    : hierize' cs

mkTest modnm plugin api ty sym = 
       "module "++ modnm ++" where" ++
       "\nimport qualified " ++ plugin  ++
       "\nimport qualified " ++ api     ++
       "{-# LINE 1 \"<typecheck>\" #-}" ++
       "\n_ = "++ plugin ++"."++ sym ++" :: "++ty

------------------------------------------------------------------------
{-
--
-- old version that tried to rip stuff from .hi files
--
pdynload obj incpaths pkgconfs sym ty = do
        (m, v) <- load obj incpaths pkgconfs sym
        ty'    <- mungeIface sym obj
        if ty == ty' 
                then return $ Just (m, v)
                else return Nothing             -- mismatched types

   where 
        -- grab the iface output from GHC. find the line relevant to our
        -- symbol. grab the string rep of the type.
        mungeIface sym o = do
                let hi = replaceSuffix o hiSuf
                (out,_) <- exec ghc ["--show-iface", hi]
                case find (\s -> (sym ++ " :: ") `isPrefixOf` s) out of
                        Nothing -> return undefined
                        Just v  -> do let v' = drop 3 $ dropWhile (/= ':') v
                                      return v'

-}

{-
--
-- a version of load the also unwraps and types a Dynamic object
--
dynload2 :: Typeable a => 
           FilePath -> 
           FilePath -> 
           Maybe [PackageConf] ->
           Symbol ->  
           IO (Module, a)

dynload2 obj incpath pkgconfs sym = do
        (m, v) <- load obj incpath pkgconfs sym
        case fromDynamic v of
            Nothing -> panic $ "load: couldn't type "++(show v)
            Just a  -> return (m,a)
-}

------------------------------------------------------------------------
--
-- | unload a module (not its dependencies)
-- we have the dependencies, so cascaded unloading is possible
--
-- once you unload it, you can't 'load' it again, you have to 'reload'
-- it. Cause we don't unload all the dependencies
--
unload  :: Module -> IO ()
unload m = rmModuleDeps m >> unloadObj m

------------------------------------------------------------------------
--
-- | unload a module and its dependencies
-- we have the dependencies, so cascaded unloading is possible
--
unloadAll :: Module -> IO ()
unloadAll m = do moduleDeps <- getModuleDeps m
                 rmModuleDeps m
                 mapM_ unloadAll moduleDeps
                 unload m


--
-- | this will be nice for panTHeon, needs thinking about the interface
-- reload a single object file. don't care about depends, assume they
-- are loaded. (should use state to store all this)
--
-- assumes you've already done a 'load'
--
-- should factor the code
--
reload :: Module -> Symbol -> IO (LoadStatus a)
reload m@(Module{path = p, iface = hi}) sym = do
        unloadObj m     -- unload module (and delete)
#if DEBUG
        putStr ("Reloading "++(mname m)++" ... ") >> hFlush stdout
#endif
        m_ <- loadObject p . Object . ifaceModuleName $ hi   -- load object at path p
        let m' = m_ { iface = hi }
    
        resolveObjs (unloadAll m)
#if DEBUG
        putStrLn "done" >> hFlush stdout
#endif
        v <- loadFunction m' sym
        return $ case v of 
                Nothing -> LoadFailure ["load: couldn't find symbol <<"++sym++">>"]
                Just a  -> LoadSuccess m' a

-- ---------------------------------------------------------------------
-- This is a stripped-down version of André Pang's runtime_loader,
-- which in turn is based on GHC's ghci\/ObjLinker.lhs binding
--
--  Load and unload\/Haskell modules at runtime.  This is not really
--  \'dynamic loading\', as such -- that implies that you\'re working
--  with proper shared libraries, whereas this is far more simple and
--  only loads object files.  But it achieves the same goal: you can
--  load a Haskell module at runtime, load a function from it, and run
--  the function.  I have no idea if this works for types, but that
--  doesn\'t mean that you can\'t try it :).
--
-- read $fptools\/ghc\/compiler\/ghci\/ObjLinker.lhs for how to use this stuff
--
------------------------------------------------------------------------

-- | Call the initLinker function first, before calling any of the other
-- functions in this module - otherwise you\'ll get unresolved symbols.

-- initLinker :: IO ()
-- our initLinker transparently calls the one in GHC

--
-- | Load a function from a module (which must be loaded and resolved first).
--

loadFunction :: Module          -- ^ The module the value is in
             -> String          -- ^ Symbol name of value
             -> IO (Maybe a)    -- ^ The value you want
loadFunction (Module { iface = i }) valsym
    = loadFunction_ (ifaceModuleName i) valsym

loadFunction_ :: String
              -> String
              -> IO (Maybe a)
loadFunction_ = loadFunction__ Nothing

loadFunction__ :: Maybe String
              -> String
              -> String
              -> IO (Maybe a)
loadFunction__ pkg m valsym
   = do let symbol = prefixUnderscore++(maybe "" (\p -> encode p++"_") pkg)
                     ++encode m++"_"++(encode valsym)++"_closure"
#if DEBUG
        putStrLn $ "Looking for <<"++symbol++">>"
#endif
        ptr@(~(Ptr addr)) <- withCString symbol c_lookupSymbol
        if (ptr == nullPtr)
            then return Nothing
            else case addrToHValue# addr of
                (# hval #) -> return ( Just hval )


-- | Loads a function from a package module, given the package name,
--   module name and symbol name.
loadPackageFunction :: String -- ^ Package name, including version number.
                    -> String -- ^ Module name
                    -> String -- ^ Symbol to lookup in the module
                    -> IO (Maybe a)
loadPackageFunction pkgName modName functionName =
    do loadPackage pkgName
       resolveObjs (unloadPackage pkgName)
       loadFunction__ (Just pkgName) modName functionName

--
-- | Load a GHC-compiled Haskell vanilla object file.
-- The first arg is the path to the object file
--
-- We make it idempotent to stop the nasty problem of loading the same
-- .o twice. Also the rts is a very special package that is already
-- loaded, even if we ask it to be loaded. N.B. we should insert it in
-- the list of known packages.
--
-- NB the environment stores the *full path* to an object. So if you
-- want to know if a module is already loaded, you need to supply the
-- *path* to that object, not the name.
-- 
-- NB -- let's try just the module name.
--
-- loadObject loads normal .o objs, and packages too. .o objs come with
-- a nice canonical Z-encoded modid. packages just have a simple name.
-- Do we want to ensure they won't clash? Probably.
--

--
-- the second argument to loadObject is a string to use as the unique
-- identifier for this object. For normal .o objects, it should be the
-- Z-encoded modid from the .hi file. For archives\/packages, we can
-- probably get away with the package name
--


loadObject :: FilePath -> Key -> IO Module
loadObject p ky@(Object k)  = loadObject' p ky k 
loadObject p ky@(Package k) = loadObject' p ky k 

loadObject' :: FilePath -> Key -> String -> IO Module
loadObject' p ky k
    | ("HSrts"++sysPkgSuffix) `isSuffixOf` p = return (emptyMod p)

    | otherwise 
    = do alreadyLoaded <- isLoaded k
         when (not alreadyLoaded) $ do
              r <- withCString p c_loadObj
              when (not r) (panic $ "Could not load module `"++p++"'")
         addModule k (emptyMod p)   -- needs to Z-encode module name
         return (emptyMod p)

    where emptyMod q = Module q (mkModid q) Vanilla undefined ky

--
-- load a single object. no dependencies. You should know what you're
-- doing.
--
loadModule :: FilePath -> IO Module
loadModule obj = do
    let hifile = replaceSuffix obj hiSuf
    exists <- doesFileExist hifile
    if (not exists)
        then error $ "No .hi file found for "++show obj
        else do hiface <- readBinIface' hifile
                loadObject obj (Object (ifaceModuleName hiface))

--
-- | Load a generic .o file, good for loading C objects.
-- You should know what you're doing..
-- Returns a fairly meaningless iface value.
--
loadRawObject :: FilePath -> IO Module
loadRawObject obj = loadObject obj (Object k)
    where
        k = encode (mkModid obj)  -- Z-encoded module name

--
-- | Resolve (link) the modules loaded by the 'loadObject' function.
--
resolveObjs :: IO a -> IO ()
resolveObjs unloadLoaded
    = do r <- c_resolveObjs
         when (not r) $ unloadLoaded >> panic "resolvedObjs failed."


-- | Unload a module
unloadObj :: Module -> IO () 
unloadObj (Module { path = p, kind = k, key = ky }) = case k of
        Vanilla -> withCString p $ \c_p -> do
                removed <- rmModule name
                when (removed) $ do r <- c_unloadObj c_p 
                                    when (not r) (panic "unloadObj: failed")
        Shared  -> return () -- can't unload .so?
    where name = case ky of Object s -> s ; Package pk -> pk
--
-- | from ghci\/ObjLinker.c
--
-- Load a .so type object file.
--
loadShared :: FilePath -> IO Module
loadShared str = do
#if DEBUG
    putStrLn $ " shared: " ++ str
#endif
    maybe_errmsg <- withCString str $ \dll -> c_addDLL dll
    if maybe_errmsg == nullPtr 
        then return (Module str (mkModid str) Shared undefined (Package (mkModid str)))
        else do e <- peekCString maybe_errmsg
                panic $ "loadShared: couldn't load `"++str++"\' because "++e


--
-- Load a -package that we might need, implicitly loading the cbits too
-- The argument is the name of package (e.g.  \"concurrent\")
--
-- How to find a package is determined by the package.conf info we store
-- in the environment. It is just a matter of looking it up.
--
-- Not printing names of dependent pkgs
--
loadPackage :: String -> IO ()
loadPackage p = do
#if DEBUG
        putStr (' ':p) >> hFlush stdout
#endif
        (libs,dlls) <- lookupPkg p
        mapM_ (\l -> loadObject l (Package (mkModid l))) libs
#if DEBUG
        putStr (' ':show libs) >> hFlush stdout
        putStr (' ':show dlls) >> hFlush stdout
#endif
	mapM_ loadShared dlls



--
-- Unload a -package, that has already been loaded. Unload the cbits
-- too. The argument is the name of the package.
--
-- May need to check if it exists.
--
-- Note that we currently need to unload everything. grumble grumble.
--
-- We need to add the version number to the package name with 6.4 and
-- over. "yi-0.1" for example. This is a bug really.
--
unloadPackage :: String -> IO ()
unloadPackage pkg = do
    let pkg' = takeWhile (/= '-') pkg   -- in case of *-0.1
    libs <- liftM (\(a,_) -> (filter (isSublistOf pkg') ) a) (lookupPkg pkg)
    flip mapM_ libs $ \p -> withCString p $ \c_p -> do
                        r <- c_unloadObj c_p 
                        when (not r) (panic "unloadObj: failed")
                        rmModule (mkModid p)      -- unrecord this module 

--
-- load a package using the given package.conf to help
-- TODO should report if it doesn't actually load the package, instead
-- of mapM_ doing nothing like above.
--
loadPackageWith :: String -> [PackageConf] -> IO ()
loadPackageWith p pkgconfs = do
#if DEBUG
        putStr "Loading package" >> hFlush stdout
#endif
        mapM_ addPkgConf pkgconfs
        loadPackage p
#if DEBUG
        putStrLn " done"
#endif
        

-- ---------------------------------------------------------------------
-- module dependency loading
--
-- given an Foo.o vanilla object file, supposed to be a plugin compiled
-- by our library, find the associated .hi file. If this is found, load
-- the dependencies, packages first, then the modules. If it doesn't
-- exist, assume the user knows what they are doing and continue. The
-- linker will crash on them anyway. Second argument is any include
-- paths to search in
--
-- ToDo problem with absolute and relative paths, and different forms of
-- relative paths. A user may cause a dependency to be loaded, which
-- will search the incpaths, and perhaps find "./Foo.o". The user may
-- then explicitly load "Foo.o". These are the same, and the loader
-- should ignore the second load request. However, isLoaded will say
-- that "Foo.o" is not loaded, as the full string is used as a key to
-- the modenv fm. We need a canonical form for the keys -- is basename
-- good enough?
--
loadDepends :: FilePath -> [FilePath] -> IO (ModIface,[Module])
loadDepends obj incpaths = do
    let hifile = replaceSuffix obj hiSuf
    exists <- doesFileExist hifile
    if (not exists)
        then do
#if DEBUG
                putStrLn "No .hi file found." >> hFlush stdout
#endif
                return (undefined,[])   -- could be considered fatal

        else do hiface <- readBinIface' hifile
                let ds = mi_deps hiface

                -- remove ones that we've already loaded
                ds' <- filterM loaded . map (moduleNameString . fst) . dep_mods $ ds

                -- now, try to generate a path to the actual .o file
                -- fix up hierachical names
                let mods_ = map (\s -> (s, map (\c -> 
                        if c == '.' then '/' else c) $ s)) ds'

                -- construct a list of possible dependent modules to load
                let mods = concatMap (\p -> 
                            map (\(hi,m) -> (hi,p </> m++".o")) mods_) incpaths

                -- remove modules that don't exist
                mods' <- filterM (\(_,y) -> doesFileExist y) $
                                nubBy (\v u -> snd v == snd u)  mods

                -- now remove duplicate valid paths to the same object
                let mods'' = nubBy (\v u -> fst v == fst u)  mods'

                -- and find some packages to load, as well.
                let ps = dep_pkgs ds
                ps' <- filterM loaded . map packageIdString . nub $ ps

#if DEBUG
                when (not (null ps')) $
                        putStr "Loading package" >> hFlush stdout
#endif
                mapM_ loadPackage ps'
#if DEBUG
                when (not (null ps')) $
                        putStr " ... linking ... " >> hFlush stdout
#endif
                resolveObjs (mapM_ unloadPackage ps')
#if DEBUG
                when (not (null ps')) $ putStrLn "done" 
                putStr "Loading object" 
                mapM_ (\(m,_) -> putStr (" "++ m) >> hFlush stdout) mods''
#endif
                moduleDeps <- mapM (\(hi,m) -> loadObject m (Object hi)) mods''
                return (hiface,moduleDeps)

-- ---------------------------------------------------------------------
-- Nice interface to .hi parser
--
getImports :: String -> IO [String]
getImports m = do
        hi <- readBinIface' (m ++ hiSuf)
        return . map (moduleNameString . fst) . dep_mods . mi_deps $ hi

-- ---------------------------------------------------------------------
-- C interface
--
foreign import ccall threadsafe "lookupSymbol"
   c_lookupSymbol :: CString -> IO (Ptr a)

foreign import ccall unsafe "loadObj"
   c_loadObj :: CString -> IO Bool

foreign import ccall unsafe "unloadObj"
   c_unloadObj :: CString -> IO Bool

foreign import ccall unsafe "resolveObjs"
   c_resolveObjs :: IO Bool

foreign import ccall unsafe "addDLL"
   c_addDLL :: CString -> IO CString

foreign import ccall unsafe "initLinker"
   initLinker :: IO ()
