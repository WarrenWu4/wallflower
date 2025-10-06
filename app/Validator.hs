module Validator
  ( checkHyprland,
    checkHyprpaper,
  )
where

import Control.Monad (unless)
import System.Directory (doesFileExist, getHomeDirectory, findExecutable)
import Data.Maybe (isJust)
import LoggerGe

-- all program depedency checks

-- | ensures that hyprland is installed and configured
-- if not crash & send error to user to switch
checkHyprland :: IO ()
checkHyprland = do
  hyprPath <- findExecutable "hyprland"
  unless (isJust hyprPath) $ logMsg ERROR "Hyprland not found in PATH" >> error "Hyprland not found in PATH"
  homeDir <- getHomeDirectory
  let temp = homeDir <> "/.config/hypr/hyprland.conf"
  configExists <- doesFileExist temp 
  unless configExists $ logMsg ERROR "Hyprland config not found" >> error "Hyprland config not found"
  logMsg OK "Hyprland check passed"

-- | checks that hyprpaper is installed and configured
-- if not crash & send error to user to setup
checkHyprpaper :: IO ()
checkHyprpaper = do
  hyprPaperPath <- findExecutable "hyprpaper"
  unless (isJust hyprPaperPath) $ logMsg ERROR "Hyprpaper not found in PATH" >> error "Hyprpaper not found in PATH"
  homeDir <- getHomeDirectory
  let temp = homeDir <> "/.config/hypr/hyprpaper.conf"
  configExists <- doesFileExist temp 
  unless configExists $ logMsg ERROR "Hyprpaper config not found" >> error "Hyprpaper config not found"
  logMsg OK "Hyprpaper check passed"

