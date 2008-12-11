page =  <div>
          <form action=(getViewDataValue_u "save-url" :: View String) method="post">
            <div>
              Title: 
              <input type="text" id="title" name="title" value=(getViewDataValue_u "page-title" :: View String) />
              </div>
              <div>
              Content:
              <textarea rows="25" columns="80" name="content" id="content">
                <% (getViewDataValue_u "page-content" :: View String) %>
              </textarea>
            </div>
            <input type="submit" value="Save"/>
          </form>
        </div>
