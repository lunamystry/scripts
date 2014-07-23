import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import System.Environment (getArgs)
import System.IO (readFile)
import Data.List (isInfixOf, any, null)


main :: IO ()
main = do
  args <- getArgs
  if null args then
    mapDir printTodos "."
  else
    mapM_ printTodos args


mapDir:: (FilePath -> IO ()) -> FilePath -> IO ()
mapDir fn name = do
    isFile <- doesFileExist name
    if isFile then fn name
    else getDirectoryContents name >>=
        mapM_ (mapDir fn . (name </>)) . filter (`notElem` [".", ".."])


printTodos:: String -> IO ()
printTodos filename = do
    content <- readFile filename
    putStrLn ("\nTODOs in the file: " ++ filename)
    mapM_ putStrLn $ todoLines $ numberedLines $ lines content


todoLines:: [String] -> [String]
todoLines = filter isTodoLine
    where
        isTodoLine line = any (`isInfixOf` line) ["TODO", "BUG"]

numberedLines:: [String] -> [String]
numberedLines lines = [(++) (show $ snd line) ((++) " " (fst line)) | line <- zip lines [1..length lines]]
