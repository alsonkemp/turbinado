module Config.Routes where

--
-- Import modules for which you'll be creating static routes.
--
import App.Layouts.Default
import App.Controllers.Home
import App.Controllers.Develop
import App.Views.Home.Index
import App.Views.Develop.Index

--
-- Configure dynamic routes for on-the-fly compiled-and-loaded
-- modules (ala Rails)
--
routes = [ "/:controller/:action/:id.:format"
         , "/:controller/:action/:id"
         , "/:controller/:action.:format"
         , "/:controller/:action"
         , "/:controller"
         , "/home"
         ]

--
-- Statically compile and load these Layouts, Controllers and Views
--
staticLayouts =
    [ ("App/Layouts/Default.hs",     "markup", App.Layouts.Default.markup)
    ]

staticControllers = 
    [ ("App/Controllers/Home.hs",    "index", App.Controllers.Home.index)
    , ("App/Controllers/Develop.hs", "index", App.Controllers.Develop.index)
    ]

staticViews =
    [ ("App/Views/Home/Index.hs",    "markup", App.Views.Home.Index.markup)
    , ("App/Views/Develop/Index.hs", "markup", App.Views.Develop.Index.markup)
    ]
