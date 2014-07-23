import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import System.IO (readFile)
import Data.List (isInfixOf, any, null)


main :: IO ()
main = do
  args <- getArgs
  content <- getDirectoryContents "."
  if null args then
    mapM_ printTodos $ filter (`notElem` [".", ".."]) content
  else
    mapM_ printTodos args


printTodos:: String -> IO ()
printTodos filename = do
    content <- readFile filename
    putStrLn ("TODOs in the file: " ++ filename)
    mapM_ print $ todoLines $ lines content


todoLines:: [String] -> [String]
todoLines = filter isTodoLine
    where
        isTodoLine line = any (`isInfixOf` line) ["TODO", "BUG"]
