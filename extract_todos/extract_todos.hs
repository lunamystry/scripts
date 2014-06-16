import System.Environment (getArgs)

main = do
  args <- getArgs
  mapM_ find_todos args


find_todos arg = do
    putStrLn ("Hello " ++ arg)
