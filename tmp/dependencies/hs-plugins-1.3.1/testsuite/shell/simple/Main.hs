import System.Plugins
import StringProcessorAPI
import System.Console.Readline
import System.Exit

source = "Plugin.hs"
stub   = "Plugin.stub"
symbol = "resource"

main = do s <- makeWith source stub []
          o <- case s of
                MakeSuccess _ obj -> do
                        ls <- load obj ["."] [] symbol
                        case ls of LoadSuccess m v -> return (m,v)
                                   LoadFailure err -> error "load failed"
                MakeFailure e -> mapM_ putStrLn e >> error "compile failed"
          shell o

shell o@(m,plugin) = do 
        s <- readline "> "
        cmd <- case s of 
                Nothing          -> exitWith ExitSuccess
                Just (':':'q':_) -> exitWith ExitSuccess
                Just s           -> addHistory s >> return s

        s  <- makeWith source stub []   -- maybe recompile the source
        o' <- case s of
                MakeSuccess ReComp o -> do 
                        ls <- reload m symbol
                        case ls of LoadSuccess m' v' -> return (m',v')
                                   LoadFailure err   -> error "reload failed"
                MakeSuccess NotReq _ -> return o
                MakeFailure e -> mapM_ putStrLn e >> shell o
        eval cmd o'
        shell o'

eval ":?" _ = putStrLn ":?\n:q\n<string>"

eval s (_,plugin) = let fn = (stringProcessor plugin) in putStrLn (fn s)


