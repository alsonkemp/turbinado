index :: Controller ()
index  = do pages <- quickQuery' "select * from page" []
            return ()

show :: Controller ()
show  = do e <- getEnvironment
           let id' = getSetting "id" e :: Maybe String
           doIO $ debugM e $ "XXXXXXXX    id' = " ++ (Prelude.show id')
           case id' of
             Nothing -> redirectTo "/Home"
             Just i  -> do q <- quickQuery' "select title, content from page where id = ?" [toSql i]
                           e' <- doIO $ setViewDataValue "page-title" (Prelude.show $ (head q) !! 0) e
                           e''<- doIO $ setViewDataValue "page-content" (Prelude.show $ (head q) !! 1) e
                           put e''


