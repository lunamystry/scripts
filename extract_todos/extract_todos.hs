import System.Environment (getArgs)
import System.IO (readFile)
import Control.Applicative


main :: IO ()
main = do
  args <- getArgs
  mapM_ print_todos args


print_todos:: String -> IO ()
print_todos filename = do
    putStrLn ("reading: " ++ filename)
    content <- readFile filename
    mapM_ print $ lines content
