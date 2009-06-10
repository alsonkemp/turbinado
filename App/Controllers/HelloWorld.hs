module App.Controllers.HelloWorld where

import Turbinado.Controller

index :: Controller ()
index = do setViewDataValue "sample_value" "smarfle!"

