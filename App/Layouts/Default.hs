import Control.Monad.Trans
import Data.List
import Data.Maybe
import qualified Network.HTTP as HTTP
import qualified Network.URI  as URI

page = <html>
         <head>
           <% styleSheet "normalize" "screen" %>
           <% styleSheet "pressurized" "screen" %>
           <% styleSheet "turbinado" "screen" %>
           <% javaScript "jquery" %>
           <% javaScript "jsddm" %>
           <% googleAnalytics "UA-6158816-1" %>
         </head>
         <body>
           <div id="wrapper">
             <div id="header">
               <div id="logo">
                 <h1>
                   <a href="http://www.turbinado.org">
                     <img src="/images/turbinado.jpg" />
                     <span style="left:140px; position:absolute; top:65px;">
                       Turbinado
                     </span>
                   </a>
                 </h1>
               </div>
             </div>
             <div id="menu">
               <ul>
                 <% menuItem "/Home/Index"         "Home" %>
                 <% menuItem "/Home/Performance"   "Performance" %>
                 <% menuItem "/Home/Architecture"  "Architecture" %>
                 <% menuItem "/Home/Install"       "Install" %>
                 <% menuItem "/Tutorial/Index"     "Tutorial" %>
                 <% menuItem "/Develop/Index"      "Develop" %>
               </ul>
             </div>
             <div id="page">
               <div id="content">
                 <% insertView %>
               </div>
             </div>
             <div style="clear: both;" />
           </div>
           <div id="footer">
             <p>Copyright (c) 2008 Turbinado.org. All rights reserved.</p>
             <p>Design by <a href="http://www.freecsstemplates.org/">Free CSS Templates</a>.</p>
           </div>
         </body>
       </html>

menuItem :: FilePath -> String -> View XML
menuItem p t = do e <- getEnvironment
                  let ru = HTTP.rqURI $ fromJust $ getRequest e
                      active = if isPrefixOf p (URI.uriPath ru) then "active" else ""
                  <li class=active><a href=p><%t%></a></li>
