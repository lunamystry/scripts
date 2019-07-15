{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Control.Foldl as Fold
import qualified Data.List     as L
import qualified Data.Text     as DT
import           Prelude       hiding (FilePath)
import           Turtle

pathToText :: FilePath -> Text
pathToText = format fp

wallpaper :: FilePath -> FilePath
wallpaper base = base </> "Images/wallpapers/wallpaper"

layoutsDir :: FilePath -> FilePath
layoutsDir base = base </> ".screenlayout"

setWallpaper :: FilePath -> Shell ()
setWallpaper w = procs "feh" ["--bg-scale", pathToText w] empty

getLayoutsScripts :: [Text] -> FilePath -> IO [FilePath]
getLayoutsScripts args dir = case args of
  [ x ]  -> fold (find (contains $ text x) dir) Fold.list
  x : xs -> fold (find (contains $ text x) dir) Fold.list
  _      -> fold (ls dir) Fold.list

filterScripts :: [FilePath] -> Shell Line
filterScripts scripts = do
  inshell (pipeToFzf scripts) empty
 where
  listToText xs = DT.pack $ concat $ L.intersperse "\n" $ encodeString <$> xs
  pipeToFzf names = "echo \"" <> listToText names <> "\" | fzf -1"

main :: IO ()
main = sh $ do
  args    <- arguments
  homeDir <- home
  scripts <- liftIO $ getLayoutsScripts args $ layoutsDir homeDir
  case scripts of
    []  -> echo "No matching layouts"
    [x] -> shells (pathToText x) empty
    xs  -> (filterScripts xs) >>= \s -> shells (lineToText s) empty
  setWallpaper $ wallpaper homeDir
