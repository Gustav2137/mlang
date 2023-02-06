import System.Environment
import System.Exit
import Parser
import Abstract
import Text.Megaparsec (errorBundlePretty)

version = "1.0"

main = getArgs >>= parse

parse ["-h"] = putStrLn "git gud \n\n\n jk wip" >> exit
parse ["-v"] = putStrLn ("Mlang interpreter " ++ version) >> exit
parse [] = do
  putStrLn "Running in interactive mode"
  putStrLn "todo"
  exit
parse (f:_) = do 
  source <- readFile f 
  case Parser.run f source of
    Left e     -> error $ errorBundlePretty e
    Right code -> Abstract.run code

exit = exitSuccess 
