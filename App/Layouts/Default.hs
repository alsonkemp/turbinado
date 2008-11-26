page :: View XML
page = <html>
         <head>
           <% styleSheet "normalize" "screen" %>
           <% styleSheet "jsddm" "screen" %>
           <% styleSheet "turbinado" "screen" %>
           <% javaScript "jquery" %>
           <% javaScript "jsddm" %>
           <% googleAnalytics "UA-6158816-1" %>
         </head>
         <body>
           <table class="wrapper">
             <tr>
               <td class="title">
                 <h1>Turbinado</h1>
                 <img class="title-image" src="/images/turbinado.jpg" />
                 <h2>Sugar For</h2>
                 <h2>The Web</h2>
               </td>
               <td class="container">
                 <ul id="jsddm">
                   <li><a href="/Home/Index">Home</a>
                     <ul>
                       <li><a href="/Home/About">About</a></li>
                       <li><a href="/Home/Performance">Performance</a></li>
                     </ul>
                   </li>
                   <li><a href="/Tutorial/Index">Tutorial</a>
                   </li>
                   <li><a href="/Develop/Index">Develop</a></li>
                 </ul>
                 <div class="clear"></div>
                 <% breadCrumbs %>
                 <div id="content-block" class="content-block">
                   <% insertView %>
                 </div>
               </td>
             </tr>
             <tr>
               <td colspan="2">
                 <div class="footer">Turbinado - www.turbinado.org</div>
               </td>
             </tr>
           </table>
         </body>
       </html>
