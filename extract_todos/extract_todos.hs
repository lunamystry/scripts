import System.Environment (getArgs)
import System.IO (readFile)
import Data.List (isInfixOf, any)


main :: IO ()
main = do
  args <- getArgs
  mapM_ print_todos args


print_todos:: String -> IO ()
print_todos filename = do
    content <- readFile filename
    putStrLn ("TODOs in the file: " ++ filename)
    mapM_ print $ todoLines $ lines content


todoLines = filter isTodoLine
    where
        isTodoLine line = any (`isInfixOf` line) ["TODO", "BUG"]
