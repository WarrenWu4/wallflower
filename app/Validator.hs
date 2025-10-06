module Validator
  ( checkHyprland,
    checkHyprpaper,
  )
where

import Control.Monad (unless)
import System.Directory (doesFileExist, getHomeDirectory, findExecutable)
import Data.Maybe (isJust)

-- all program depedency checks

-- | ensures that hyprland is installed and configured
-- if not crash & send error to user to switch
checkHyprland :: IO ()
checkHyprland = do
  hyprPath <- findExecutable "hyprland"
  unless (isJust hyprPath) $ error "Hyprland not found in PATH"
  homeDir <- getHomeDirectory
  let temp = homeDir <> "/.config/hypr/hyprland.conf"
  configExists <- doesFileExist temp 
  unless configExists $ error "Hyprland config not found"
  print "Hyprland check passed"

-- | checks that hyprpaper is installed and configured
-- if not crash & send error to user to setup
checkHyprpaper :: IO ()
checkHyprpaper = do
  hyprPaperPath <- findExecutable "hyprpaper"
  unless (isJust hyprPaperPath) $ error "Hyprpaper not found in PATH"
  homeDir <- getHomeDirectory
  let temp = homeDir <> "/.config/hypr/hyprpaper.conf"
  configExists <- doesFileExist temp 
  unless configExists $ error "Hyprpaper config not found"
  print "Hyprpaper check passed"

