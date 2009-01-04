#!/usr/bin/env runhaskell
> module Main where
> import Distribution.Simple
> import Distribution.Setup   ( ConfigFlags (..) )
> import System.Directory     ( findExecutable )
> 
> main :: IO ()
> main = defaultMainWithHooks (defaultUserHooks { postConf = defaultPostConf })
>     where defaultPostConf args flags lbi {- xx -}
>               = do args' <- fmap (args++) (configToArgs flags)
>                    (postConf defaultUserHooks) args' flags lbi {- xx -}
> 
> -- need to pass with-ghc arg onto ./configure for non-standard ghcs
> configToArgs :: ConfigFlags -> IO [String]
> configToArgs (ConfigFlags { configHcPath = Just hcPath })
>     = do exec <- findExecutable hcPath
>          case exec of
>            Just realPath -> return ["--with-ghc="++realPath]
>            Nothing -> return ["--with-ghc="++hcPath]
> configToArgs _ = return []
