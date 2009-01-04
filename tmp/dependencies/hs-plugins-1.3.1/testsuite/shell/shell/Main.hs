--
-- a simple shell for loading plugins and evaluating their functions
-- 

import System.Plugins
import API

import Data.Either
import Data.Char
import Control.Monad            ( when )
import System.Console.Readline  ( readline )
import System.Exit              ( ExitCode(..), exitWith )


source = "Plugin.hs"
stub   = "Plugin.stub"

sym = "resource"

main = do
        status <- makeWith source stub []
        p <- case status of
                MakeFailure e -> mapM_ putStrLn e >> error "failed to compile"
                MakeSuccess _ obj -> do
                        m_v <- load obj ["."] [] sym
                        case m_v of
                                LoadSuccess m v -> return (m,v)
                                LoadFailure e   -> do mapM_ putStrLn e
                                                      error "failed to load"
        shell p

    where
        shell p@(m,v) = do 

            s <- readline "> "
            cmd <- case s of 
                Nothing   -> exitWith ExitSuccess
                Just ":q" -> exitWith ExitSuccess
                Just s    -> return (chomp s)

            status <- makeWith source stub []   
            case status of 
                MakeFailure e -> do 
                    mapM_ putStrLn e
                    shell p     -- print error and back to prompt

                MakeSuccess NotReq o ->  do
                    p' <- eval cmd p
                    shell p'    -- eval str again

                MakeSuccess ReComp o -> do
                    m_v' <- reload m sym
                    case m_v' of
                        LoadFailure e    -> mapM_ putStrLn e >> error "failed to load"
                        LoadSuccess _ v' -> do
                                let p' = (m,v')
                                p'' <- eval cmd p'
                                shell p''

--
-- shell commands
--
eval "" p = return p

eval ":clear" p = do
        let loop i = when (i < 40) (do putStr "\n" ; loop $! i+1)
        loop 0
        return p

eval ":?"  p     = do
        putStrLn$"\":?\"\n"       ++
                 "\":quit\"\n"    ++
                 "\":clear\"\n"   ++
                 "\"foo\""
        return p

eval s (m,v) = putStrLn ((function v) s) >> return (m,v)

--
-- strip trailing whitespace
--
chomp :: String -> String
chomp [] = []
chomp s | isSpace (last s) = chomp $! init s
        | otherwise        = s
