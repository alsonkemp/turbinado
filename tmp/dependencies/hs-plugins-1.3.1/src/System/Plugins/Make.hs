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

-- | An interface to a Haskell compiler, providing the facilities of a
-- compilation manager.

module System.Plugins.Make ( 

        -- * The @MakeStatus@ type
        MakeStatus(..),

        -- * The @MakeCode@ type
        MakeCode(..),

        -- * Compiling Haskell modules
        make, 
        makeAll,
        makeWith, 

        -- * Handling reecompilation
        hasChanged,
        hasChanged',
        recompileAll,
        recompileAll',

        -- * Merging together Haskell source files 
        MergeStatus(..),
        MergeCode,
        Args,
        Errors,
        merge, 
        mergeTo,
        mergeToDir,

        -- * Cleaning up temporary files
        makeClean,
        makeCleaner,

        -- * Low-level compilation primitives
        build, {- internal -}

  ) where

import System.Plugins.Utils
import System.Plugins.Parser
import System.Plugins.LoadTypes        ( Module (Module, path) )
import System.Plugins.Consts           ( ghc, hiSuf, objSuf, hsSuf )
import System.Plugins.Process          ( exec )
import System.Plugins.Env              ( lookupMerged, addMerge
                                       , getModuleDeps)

#if DEBUG
import System.IO (hFlush, stdout, openFile, IOMode(..),hClose, hPutStr, hGetContents)
#else
import System.IO (openFile, IOMode(..),hClose,hPutStr, hGetContents)
#endif

import System.Directory         ( doesFileExist, removeFile
                                , getModificationTime )

import Control.Exception        ( handleJust )
import GHC.IOBase               ( Exception(IOException) )

#if __GLASGOW_HASKELL__ >= 604
import System.IO.Error          ( isDoesNotExistError )
#endif

--
-- | The @MakeStatus@ type represents success or failure of compilation.
-- Compilation can fail for the usual reasons: syntax errors, type
-- errors and the like. The @MakeFailure@ constructor returns any error
-- messages produced by the compiler. @MakeSuccess@ returns a @MakeCode@
-- value, and the path to the object file produced.
--
data MakeStatus 
        = MakeSuccess MakeCode FilePath     -- ^ compilation was successful
        | MakeFailure Errors                -- ^ compilation failed
        deriving (Eq,Show)

-- | The @MakeCode@ type is used when compilation is successful, to
-- distinguish two cases: 
--  * The source file needed recompiling, and this was done
--  * The source file was already up to date, recompilation was skipped
data MakeCode   
    = ReComp    -- ^ recompilation was performed
    | NotReq    -- ^ recompilation was not required
        deriving (Eq,Show)

--
-- | An equivalent status for the preprocessor phase
--
data MergeStatus 
        = MergeSuccess MergeCode Args FilePath  -- ^ the merge was successful
        | MergeFailure Errors                   -- ^ failure, and any errors returned
        deriving (Eq,Show)

-- 
-- | Merging may be avoided if the source files are older than an
-- existing merged result. The @MergeCode@ type indicates whether
-- merging was performed, or whether it was unneccessary.
--
type MergeCode = MakeCode

-- | A list of @String@ arguments
type Args   = [Arg]

-- | Convience synonym
type Errors = [String]

-- touch.

-- ---------------------------------------------------------------------
-- | One-shot unconditional compilation of a single Haskell module.
-- @make@ behaves like 'ghc -c'. Extra arguments to 'ghc' may be passed
-- in the 'args' parameter, they will be appended to the argument list.
-- @make@ always recompiles its target, whether or not it is out of
-- date.
-- 
-- A side-effect of calling 'make' is to have GHC produce a @.hi@ file
-- containing a list of package and objects that the source depends on.
-- Subsequent calls to 'load' will use this interface file to load
-- module and library dependencies prior to loading the object itself.
--
make :: FilePath -> [Arg] -> IO MakeStatus
make src args = rawMake src ("-c":args)  True

-- | 'makeAll' recursively compiles any dependencies it can find using
-- GHC's @--make@ flag. Dependencies will be recompiled only if they are
-- visible to 'ghc' -- this may require passing appropriate include path
-- flags in the 'args' parameter. 'makeAll' takes the top-level file as
-- the first argument.
--
makeAll :: FilePath -> [Arg] -> IO MakeStatus
makeAll src args = 
    rawMake src ( "--make":"-no-hs-main":"-no-link":"-v0":args ) False

-- | This is a variety of 'make' that first calls 'merge' to
-- combine the plugin source with a syntax stub. The result is then
-- compiled. This is provided for EDSL authors who wish to add extra
-- syntax to a user\'s source. It is important to note that the
-- module and types from the second file argument are used to override
-- any of those that appear in the first argument. For example, consider
-- the following source files:
--
-- > module A where
-- > a :: Integer
-- > a = 1
--
-- and
-- 
-- > module B where
-- > a :: Int
--
-- Calling @makeWith "A" "B" []@ will merge the module name and types
-- from module B into module A, generating a third file:
--
-- > {-# LINE 1 "A.hs" #-}
-- > module MxYz123 where
-- > {-# LINE 3 "B.hs" #-}
-- > a :: Int
-- > {-# LINE 4 "A.hs" #-}
-- > a = 1
--      
makeWith :: FilePath                           -- ^ a src file
         -> FilePath                           -- ^ a syntax stub file
         -> [Arg]                              -- ^ any required args
         -> IO MakeStatus                      -- ^ path to an object file

makeWith src stub args = do
    status <- merge src stub
    case status of
        MergeFailure errs -> return $ MakeFailure ("merge failed:\n":errs)
        MergeSuccess _ args' tmpf -> do
                 status' <- rawMake tmpf ("-c": args' ++ args) True
                 return status'

------------------------------------------------------------------------
--
-- | @hasChanged@ returns @True@ if the module or any of its
-- dependencies have older object files than source files.  Defaults to
-- @True@ if some files couldn't be located.
--
hasChanged :: Module -> IO Bool
hasChanged = hasChanged' ["hs","lhs"]

hasChanged' :: [String] -> Module -> IO Bool
hasChanged' suffices m@(Module {path = p})
    = do modFile <- doesFileExist p
         mbFile <- findFile suffices p
         case mbFile of
           Just f | modFile
             -> do srcT <- getModificationTime f
                   objT <- getModificationTime p
                   if srcT > objT
                      then return True
                      else do deps <- getModuleDeps m
                              depsStatus <- mapM (hasChanged' suffices) deps
                              return (or depsStatus)
           _ -> return True

--
-- | 'recompileAll' is like 'makeAll', but rather than relying on 
-- @ghc --make@, we explicitly check a module\'s dependencies using our
-- internal map of module dependencies. Performance is thus better, and
-- the result is more accurate.
--
recompileAll :: Module -> [Arg] -> IO MakeStatus
recompileAll = recompileAll' ["hs","lhs"]

recompileAll' :: [String] -> Module -> [Arg] -> IO MakeStatus
recompileAll' suffices m args
    = do changed <- hasChanged m
         if changed
            then do mbSource <- findFile suffices (path m)
                    case mbSource of
                      Nothing
                          -> error $ "Couldn't find source for object file: " ++ path m
                      Just source
                          -> makeAll source args
            else return (MakeSuccess NotReq (path m))

-- ---------------------------------------------------------------------
-- rawMake : really do the compilation
-- Conditional on file modification times, compile a .hs file
-- When using 'make', the name of the src file must be the name of the
-- .o file you are expecting back
--
-- Problem: we use GHC producing stdout to indicate compilation failure.
-- We should instead check the error conditions. I.e. --make will
-- produce output, but of course compiles correctly. TODO
-- So, e.g. --make requires -v0 to stop spurious output confusing
-- rawMake
--
-- Problem :: makeAll incorrectly refuses to recompile if the top level
-- src isn't new.
--

rawMake :: FilePath        -- ^ src
        -> [Arg]           -- ^ any compiler args
        -> Bool            -- ^ do our own recompilation checking
        -> IO MakeStatus

rawMake src args docheck = do
        src_exists <- doesFileExist src
        if not src_exists
                then return $ MakeFailure ["Source file does not exist: "++src]
                else do {
        ; let (obj,_) = outFilePath src args
        ; src_changed <- if docheck then src `newer` obj else return True
        ; if not src_changed
          then return $ MakeSuccess NotReq obj
          else do 
#if DEBUG    
                putStr "Compiling object ... " >> hFlush stdout
#endif
                err <- build src obj args
#if DEBUG    
                putStrLn "done"
#endif
                return $ if null err 
                         then MakeSuccess ReComp obj 
                         else MakeFailure err
        }

--
-- | Lower-level than 'make'. Compile a .hs file to a .o file
-- If the plugin needs to import an api (which should be almost
-- everyone) then the ghc flags to find the api need to be provided as
-- arguments
--
build :: FilePath          -- ^ path to .hs source
      -> FilePath          -- ^ path to object file
      -> [String]          -- ^ any extra cmd line flags
      -> IO [String]

build src obj extra_opts = do

    let odir = dirname obj -- always put the .hi file next to the .o file
                           -- does this work in the presence of hier plugins?
                           -- won't handle hier names properly.

    let ghc_opts = [ "-Onot" ]
        output   = [ "-o", obj, "-odir", odir, 
                     "-hidir", odir, "-i" ++ odir ]

    let flags = ghc_opts ++ output ++ extra_opts ++ [src]

#if DEBUG
    -- env.
    putStr $ show $ ghc : flags
#endif

    (_out,err) <- exec ghc flags       -- this is a fork()

    obj_exists <- doesFileExist obj -- sanity
    return $ if not obj_exists && null err -- no errors, but no object?
             then ["Compiled, but didn't create object file `"++obj++"'!"]
             else err

-- ---------------------------------------------------------------------
-- | Merge to source files into a temporary file. If we've tried to
-- merge these two stub files before, then reuse the module name (helps
-- recompilation checking)
--
-- The merging operation is extremely useful for providing extra default
-- syntax. An EDSL user then need not worry about declaring module
-- names, or having required imports.  In this way, the stub file can
-- also be used to provide syntax declarations that would be
-- inconvenient to require of the plugin author. 
--
-- 'merge' will include any import and export declarations written in
-- the stub, as well as any module name, so that plugin author\'s need
-- not worry about this compulsory syntax. Additionally, if a plugin
-- requires some non-standard library, which must be provided as a
-- @-package@ flag to GHC, they may specify this using the non-standard
-- @GLOBALOPTIONS@ pragma.  Options specified in the source this way
-- will be added to the command line. This is useful for users who wish
-- to use GHC flags that cannot be specified using the conventional
-- @OPTIONS@ pragma. The merging operation uses the parser hs-plugins
-- was configured with, either 'Language.Haskell' or the HSX parser, to
-- parse Haskell source files.
--
merge :: FilePath -> FilePath -> IO MergeStatus
merge src stb = do 
    m_mod <- lookupMerged src stb
    (out,domerge) <- case m_mod of
                Nothing -> do out <- mkUnique
                              addMerge src stb (dropSuffix out)
                              return (out, True) -- definitely out of date
                Just nm -> return $ (nm <> hsSuf, False)
    rawMerge src stb out domerge

-- | 'mergeTo' behaves like 'merge', but we can specify the file in
-- which to place output. 
mergeTo :: FilePath -> FilePath -> FilePath -> IO MergeStatus
mergeTo src stb out = rawMerge src stb out False

-- | 'mergeToDir' behaves like 'merge', but lets you specify a target
-- directory.
mergeToDir :: FilePath -> FilePath -> FilePath -> IO MergeStatus
mergeToDir src stb dir = do
    out <- mkUniqueIn dir
    rawMerge src stb out True

-- ---------------------------------------------------------------------
-- Conditional on file modification times, merge a src file with a
-- syntax stub file into a result file.
--
-- Merge should only occur if the srcs has changed since last time.
-- Parser errors result in MergeFailure, and are reported to the client
--
-- Also returns a list of cmdline flags found in pragmas in the src of
-- the files. This last feature exists as OPTION pragmas aren't handled
-- (for obvious reasons, relating to the implementation of OPTIONS
-- parsing in GHC) by the library parser, and, also, we want a way for
-- the user to introduce *dynamic* cmd line flags in the .conf file.
-- This is achieved via the GLOBALOPTIONS pragma : an extension to ghc
-- pragma syntax
--
rawMerge :: FilePath -> FilePath -> FilePath -> Bool -> IO MergeStatus
rawMerge src stb out always_merge = do
    src_exists <- doesFileExist src
    stb_exists <- doesFileExist stb
    case () of {_
        | not src_exists  -> return $ 
                MergeFailure ["Source file does not exist : "++src]
        | not stb_exists -> return $ 
                MergeFailure ["Source file does not exist : "++stb]
        | otherwise -> do {
    
    ;do_merge <- do src_changed <- src `newer` out
                    stb_changed <- stb `newer` out
                    return $ src_changed || stb_changed

    ;if not do_merge && not always_merge
     then return $ MergeSuccess NotReq [] out
     else do
        src_str <- readFile' src
        stb_str <- readFile' stb

        let (a,a') = parsePragmas src_str
            (b,b') = parsePragmas stb_str
            opts = a ++ a' ++ b ++ b'

        let e_src_syn = parse src src_str
            e_stb_syn = parse stb stb_str
    
        -- check if there were parser errors
        case (e_src_syn,e_stb_syn) of
                (Left e,  _)       -> return $ MergeFailure [e]
                (_ , Left e)       -> return $ MergeFailure [e]
                (Right src_syn, Right stb_syn) -> do {

        ;let mrg_syn = mergeModules src_syn stb_syn
             mrg_syn'= replaceModName mrg_syn (mkModid $ basename out)
             mrg_str = pretty mrg_syn'

        ;hdl <- openFile out WriteMode  -- overwrite!
        ;hPutStr hdl mrg_str ; hClose hdl
        ;return $ MergeSuccess ReComp opts out -- must have recreated file
    }}}

-- ---------------------------------------------------------------------
-- | makeClean : assuming we some element of [f.hs,f.hi,f.o], remove the
-- .hi and .o components. Silently ignore any missing components. /Does
-- not remove .hs files/. To do that use 'makeCleaner'. This would be
-- useful for merged files, for example.
--
makeClean :: FilePath -> IO ()
makeClean f = let f_hi = dropSuffix  f <> hiSuf
                  f_o  = dropSuffix  f <> objSuf
              in mapM_ rm_f [f_hi, f_o]

makeCleaner :: FilePath -> IO ()
makeCleaner f = makeClean f >> rm_f (dropSuffix f <> hsSuf)
           
-- internal:
--      try to remove a file, ignoring if it didn't exist in the first place
-- Doesn't seem to be able to remove all files in all circumstances, why?
--
rm_f f = handleJust doesntExist (\_->return ()) (removeFile f)
    where
        doesntExist (IOException ioe)
                | isDoesNotExistError ioe = Just ()
                | otherwise               = Nothing
        doesntExist _ = Nothing

readFile' f = do
    h <- openFile f ReadMode
    s <- hGetContents h
    length s `seq` return ()
    hClose h
    return s

