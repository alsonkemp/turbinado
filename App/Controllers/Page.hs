import App.Models.PageModel

index :: Controller ()
index  = do conn <- liftIO $ fromJust $ databaseConnection
            pages <- liftIO $ findAll conn
            setViewDataValue "pages-list" $ map (\p -> (title p, _id p)) pages

show :: Controller ()
show  = do conn <- liftIO $ fromJust $ databaseConnection
           e    <- get
           id'  <- getSetting "id"
           case id' of
             Nothing -> redirectTo "/Home"
             Just i  -> do p <- find conn i
                           setViewDataValue "page-title" (title p)
                           setViewDataValue "page-content" (content p)

new :: Controller ()
new  = do  conn <- liftIO $ fromJust $ databaseConnection
           e    <- get
           id'  <- getSetting "id"
           case id' of
             Nothing -> redirectTo "/Home"
             Just i  -> do setViewDataValue "save-url" ("/Page/Create/" ++ i)

create :: Controller ()
create = do conn <- liftIO $ fromJust $ databaseConnection
            e    <- get
            id'  <- getSetting "id"
            _title   <- getParam_u "title"
            _content <- getParam_u "content"
            case id' of
              Nothing -> redirectTo "/Home"
              Just i  -> do App.Models.PageModel.insert conn Page {authorId = Nothing,_id = i, title = _title, content = _content, version = 1}
                            redirectTo $ "/Page/Show/" ++ i
edit :: Controller ()
edit  = do conn <- liftIO $ fromJust $ databaseConnection
           e    <- get
           id'  <- getSetting "id"
           case id' of
             Nothing -> redirectTo "/Home"
             Just i  -> do p <- find conn i
                           setViewDataValue "save-url" ("/Page/Save/" ++ i)
                           setViewDataValue "page-title" (title p)
                           setViewDataValue "page-content" (content p)

save :: Controller ()
save = do   conn <- liftIO $ fromJust $ databaseConnection
            e    <- get
            id'  <- getSetting "id"
            _title   <- getParam_u "title"
            _content <- getParam_u "content"
            case id' of
              Nothing -> redirectTo "/Home"
              Just i  -> do p <- find conn i
                            App.Models.PageModel.update conn p {title = _title, content = _content}
                            redirectTo $ "/Page/Show/" ++ i
    
