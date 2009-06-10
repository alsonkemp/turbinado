import Turbinado.Database.ORM.Generator
import Config.Database


main = do putStrLn "Generation starting."
          generateModels ormAdapter
          putStrLn "Generation completed.  Models are in App/Models."
