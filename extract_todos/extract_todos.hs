import System.Directory (doesFileExist, getCurrentDirectory)
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
       mapM_ printTodos =<< allowedFiles =<< getCurrentDirectory
    else
      mapM_ printTodos args


allowedFiles:: FilePath -> IO [FilePath]
allowedFiles =
    find always (isAllowed)
    where
        isAllowed = foldl1 (&&?) $ isAllowedExtensions ++ isOtherAllowed
        isAllowedExtensions = map (extension /=?) notAllowedExtensions
        isOtherAllowed = [(fileType ==? RegularFile)]


printTodos:: String -> IO ()
printTodos filename = do
    content <- readFile filename
    mapM_ putStrLn $ withFormatting $ todoLines $ indexedLines $ lines content
    where
        todoLines = filter isTodoLine
            where
                isTodoLine line = any (`isInfixOf` line) ["TODO", "BUG"]
        indexedLines lines = zipWith (\x y -> show y ++ " " ++ x) lines [1..length lines]
        withFormatting lines = if null lines then lines
                               else ("\n" ++ filename) : (map ("\t" ++) lines)
