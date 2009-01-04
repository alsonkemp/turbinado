#!/usr/bin/env runhaskell

\begin{code}
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import qualified Distribution.Verbosity as Verbosity

main = defaultMainWithHooks defaultUserHooks {
         hookedPrograms = [pgConfigProgram],
         postConf=configure
       }

pgConfigProgram = (simpleProgram "pg_config") {
  programFindVersion = findProgramVersion "--version" $ \str ->
    -- Invoking "pg_config --version" gives a string like "PostgreSQL 8.0.13"
    case words str of
      (_:ver:_) -> 
          -- Hack: drop off the "RC" bit since Cabal doesn't like it.
          takeWhile (/= 'R')
          ver
      _         -> ""
}

configure _ _ _ lbi = do
  mb_bi <- pgConfigBuildInfo Verbosity.normal lbi
  writeHookedBuildInfo "HDBC-postgresql.buildinfo" (mb_bi,[])
\end{code}

Populate BuildInfo using pg_config tool.
\begin{code}
pgConfigBuildInfo verbosity lbi = do
  (pgConfigProg, _) <- requireProgram verbosity pgConfigProgram
                       (orLaterVersion $ Version [8] []) (withPrograms lbi)
  let pg_config = rawSystemProgramStdout verbosity pgConfigProg
  libDir       <- pg_config ["--libdir"]
  incDir       <- pg_config ["--includedir"]
  return $ Just emptyBuildInfo {
    extraLibDirs = lines libDir,
    includeDirs  = lines incDir
  }
\end{code}
