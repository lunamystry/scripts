import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import System.FilePath.Find
import System.Environment (getArgs)
import System.IO (readFile)
import Data.List (isInfixOf, any, null)

notAllowedExtensions = [".hi", ".o", ""]

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
    else find always (isAllowed) name >>=
        mapM_ (mapDir fn . (name </>)) . filter (`notElem` [".", ".."])
    where
        isAllowed = foldl1 (&&?) $ isAllowedExtensions ++ isOther
        isAllowedExtensions = map (extension /=?) notAllowedExtensions
        isOther = [fileType ==? RegularFile]


printTodos:: String -> IO ()
printTodos filename = do
    content <- readFile filename
    putStrLn ("\nin file: " ++ filename)
    mapM_ putStrLn $ todoLines $ indexedLines $ lines content
        where
            todoLines = filter isTodoLine
                where
                    isTodoLine line = any (`isInfixOf` line) ["TODO", "BUG"]
            indexedLines lines = zipWith (\x y -> show y ++ " " ++ x) lines [1..length lines]
