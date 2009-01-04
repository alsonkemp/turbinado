
import System.Plugins
import API

main = do
        m_v   <- dynload "../TestIO.o" ["../api"] 
                                       [] "resource_dyn"
        case m_v of
                LoadFailure _   -> error "couldn't compile"
                LoadSuccess _ v -> do 
                        s <- field v
                        if s /= [] then print True else print False
