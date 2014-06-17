import System.Environment (getArgs)
import System.IO (readFile)
import Data.List (isInfixOf)
import Control.Applicative


main :: IO ()
main = do
  args <- getArgs
  mapM_ print_todos args


print_todos:: String -> IO ()
print_todos filename = do
    content <- readFile filename
    putStrLn ("TODOs in the file: " ++ filename)
    mapM_ print $ filter isTodoLine $ lines content


isTodoLine:: String -> Bool
isTodoLine = isInfixOf "TODO"

