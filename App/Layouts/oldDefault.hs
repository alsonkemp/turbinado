module Default (page) where
import qualified Network.URI as URI
import qualified Network.HTTP as HTTP

page :: View XML
page =     <html>
             <head>
              <% styleSheet "extjs/css/ext-all" "screen"%>
              <% styleSheet "turbinado" "screen"%>

              <% javaScript "extjs/adapter/ext/ext-base" %>
              <% javaScript "ext-all" %>
              <% googleAnalytics "UA-6158816-1" %>

              <script type="text/javascript">
              Ext.onReady(function(){
                var tabs = new Ext.TabPanel({
                  renderTo: 'content-block',
                  //width:450,
                  activeTab: 0,
                  frame:true,
                  autoHeight: true,
                  autoWidth: true,
                  layout: 'fit',
                  defaults:{autoHeight: true},
                  items:[
                     {autoLoad: '/Home/Index.xml',     title: 'Home'}
                   , {autoLoad: '/Develop/Index.xml',  title: 'Develop'}
                   , {autoLoad: '/Tutorial/Index.xml', title: 'Tutorial'}
                   , {autoLoad: '/Code/Index.xml',     title: 'Code'}
                   ]
                });
              });
              </script>
             </head>
             <body>
               <div class="wrapper">
                 <div class="title"> 
                   <h1 style="display: inline">Turbinado</h1> 
                   <h2 style="display: inline">web sugar</h2> 
                 </div>
                 <hr />
                 <div class="container">
                   <div id="content-block" class="content-block">
                   </div>
                 </div>
                 <hr id="hr-footer" />
                 <div class="footer">
                   Footer
                 </div>
               </div>
             </body>
           </html>

anchorWithImage :: String -> String -> View XML
anchorWithImage l i = <a href=l>
                        <img src=i height="100" />
                      </a>

anchorWithText :: String -> String -> View XML
anchorWithText l t = <a href=l>
                       <% t %>
                     </a>

