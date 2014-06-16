import System.Environment (getArgs)
import System.IO (readFile)
import Control.Applicative


main :: IO ()
main = do
  args <- getArgs
  mapM_ find_todos args


find_todos:: String -> IO ()
find_todos filename = do
    putStrLn ("reading: " ++ filename)
    content <- tail <$> (readFile filename)
    putStrLn (content)
