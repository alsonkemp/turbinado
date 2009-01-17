
import Turbinado.Database.ORM.Generator
import Config.Master

main = do putStrLn "Generation starting."
          generateModels
          putStrLn "Generation completed.  Models are in App/Models."