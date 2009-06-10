module App.Layouts.Default where

import Turbinado.Layout

markup = tag "html" $
          tag "body" $
            insertDefaultView 


