module Config.Routes where

routes = [ "/:controller/:action/:id"
         , "/:controller/:action.:format"
         , "/:controller/:action"
         , "/:controller"
         ]
