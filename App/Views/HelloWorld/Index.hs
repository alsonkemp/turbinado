module App.Views.HelloWorld.Index where
import Turbinado.View

markup :: VHtml
markup=
  <div>
    <% someTextXHtml %>
    <% someHtml %>
    <% someHAML %>
  </div>

-- | These are the raw Turbinado.View.Html style tags
someTextXHtml :: VHtml
someTextXHtml = do s <- getViewDataValue_u "sample_value" :: View String
                   ((tag "div") (
                    ((tag "i") (stringToVHtml s)) +++
                    ((tag "b") (stringToVHtml "some text in Turbinado.View.Html style"))
                    )) 
                    
someHtml :: VHtml
someHtml = do s <- getViewDataValue_u "sample_value" :: View String
              <div>
                <i><%= s %></i>
                <b>some text in XHtml style</b>
              </div>

someHAML :: VHtml
someHAML = do s <- getViewDataValue_u "sample_value" :: View String
              %div
                %i= s
                %b some text in HAML style


