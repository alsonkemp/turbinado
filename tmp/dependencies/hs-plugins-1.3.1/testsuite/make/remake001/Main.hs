--
-- expected output:
-- $ ./a.out 
-- True
-- False
-- True
-- False
--

import System.Plugins
import System.Directory
   
main = do
        status <- make "Foo.hs" [] -- should make
        print status

        status <- make "Foo.hs" [] -- shouldn't make
        print status
   
        status <- merge "Foo.hs" "Bar.hs"
        case status of
                MergeFailure e    -> error $ show e
                MergeSuccess _ _ fp -> do {

        ;status <- make fp []       -- should make
        ;() <- case status of 
                MakeSuccess c _ -> print c
                MakeFailure e   -> error $ show e

        ;status <- make fp []       -- shouldn't make
        ;case status of 
                MakeSuccess c _ -> print c
                MakeFailure e   -> error $ show e
        ;removeFile "Foo.o"
        }

