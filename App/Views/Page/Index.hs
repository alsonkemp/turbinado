page =  <div>
          <h1>
            Page Index
          </h1>
          <% (getViewDataValue_u "pages-list" :: View [(String, String)]) >>= 
               \l ->  mapM indexItem l %>
        </div>

indexItem (t,i) = return $ cdata $ unlines $
                   ["<div style='padding: 0pt 5px;'>"
                   ," <a href=\"/Page/Show/" ++ i ++"\">"
                   ,"  "++ t
                   ," </a>"
                   ,"</div>"
                   ]
