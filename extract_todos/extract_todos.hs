import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  mapM_ find_todos args


find_todos:: String -> IO ()
find_todos arg = do
    putStrLn ("Hello " ++ arg)
