
import Turbinado.Database.ORM.Generator
import Config.Master

main = do putStrLn "Generation starting."
          generateModels
          putStrLn "Generation completed."
          putStrLn "Have a gander at App/Models."
