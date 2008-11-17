
index :: Controller ()
index = return ()

about :: Controller ()
about = return ()

performance :: Controller ()
performance = return ()

hello :: Controller ()
hello = do e  <- getEnvironment
           e' <- doIO $ clearLayout e
           put e'


