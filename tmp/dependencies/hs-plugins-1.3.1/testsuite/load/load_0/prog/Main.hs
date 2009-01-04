
import System.Plugins
import API

main = do
        m_v <- load_ "../Test.o" ["../api"] "resource"
        v <- case m_v of
                LoadFailure _   -> error "load failed"
                LoadSuccess _ v -> return v
        let s = field v
        print s
