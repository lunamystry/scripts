import System.Environment (getArgs)
import System.IO (readFile)
import Data.List (isInfixOf)
import Control.Applicative
import Control.Monad
import Control.Monad.Reader


main :: IO ()
main = do
  args <- getArgs
  mapM_ print_todos args


print_todos:: String -> IO ()
print_todos filename = do
    content <- readFile filename
    putStrLn ("TODOs in the file: " ++ filename)
    mapM_ print $ todoLines $ lines content


todoLines = filter (liftM2 (||) isTodoLine isBugLine)
    where
        isTodoLine = isInfixOf "TODO"
        isBugLine = isInfixOf "BUG"


