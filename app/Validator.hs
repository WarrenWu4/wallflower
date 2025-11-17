module Validator
  ( checkAllDependencies,
    doesHyprlandExist,
    isHyprlandRunning,
    doesHyprpaperExist,
    isHyprpaperRunning,
    doesResourceFolderExist,
    doAllIconsExist,
    doAllFontsExist,
    doesSettingsFileExist,
  )
where

import Control.Monad (unless)
import Data.Maybe (isJust)
import LoggerGe
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable, getHomeDirectory)
import Utilities

-- all program depedency checks
-- crashes if dependency not met

checkAllDependencies :: IO ()
checkAllDependencies = do
  doesHyprlandExist
  isHyprlandRunning
  doesHyprpaperExist
  isHyprpaperRunning
  doesResourceFolderExist
  doAllIconsExist
  doAllFontsExist
  doesSettingsFileExist

doesHyprlandExist :: IO ()
doesHyprlandExist = do
  hyprPath <- findExecutable "hyprland"
  unless (isJust hyprPath) $ logMsg ERROR "doesHyprlandExist check failed" >> error "Hyprland not found in PATH"
  homeDir <- getHomeDirectory
  let temp = homeDir <> "/.config/hypr/hyprland.conf"
  configExists <- doesFileExist temp
  unless configExists $ logMsg ERROR "doesHyprlandExist check failed" >> error "Hyprland config not found"
  logMsg OK "doesHyprlandExist check passed"

isHyprlandRunning :: IO ()
isHyprlandRunning = do
  running <- isProgramRunning "hyprland"
  if running
    then logMsg OK "isHyprlandRunning check passed"
    else logMsg ERROR "isHyprlandRunning check failed" >> error "Hyprland is not running"

doesHyprpaperExist :: IO ()
doesHyprpaperExist = do
  hyprPaperPath <- findExecutable "hyprpaper"
  unless (isJust hyprPaperPath) $ logMsg ERROR "doesHyprpaperExist check failed" >> error "Hyprpaper not found in PATH"
  homeDir <- getHomeDirectory
  let temp = homeDir <> "/.config/hypr/hyprpaper.conf"
  configExists <- doesFileExist temp
  unless configExists $ logMsg ERROR "doesHyprpaperExist check failed" >> error "Hyprpaper config not found"
  logMsg OK "doesHyprpaperExist check passed"

isHyprpaperRunning :: IO ()
isHyprpaperRunning = do
  running <- isProgramRunning "hyprpaper"
  if running
    then logMsg OK "isHyprpaperRunning check passed"
    else logMsg ERROR "isHyprpaperRunning check failed" >> error "Hyprpaper is not running"

doesResourceFolderExist :: IO ()
doesResourceFolderExist = do
  resPath <- getResourcePath "resources"
  exists <- doesDirectoryExist resPath
  if exists
    then logMsg OK "doesResourceFolderExist check passed"
    else logMsg ERROR "doesResourceFolderExist check failed" >> error "Resource folder not found"

doAllIconsExist :: IO ()
doAllIconsExist = do
  let icons = ["folder-icon.png", "settings-icon-d.png", "settings-icon-l.png", "wallpaper-icon-d.png", "wallpaper-icon-l.png"]
  let iconsResPath = ["resources/icons/" ++ icon | icon <- icons]
  doesResourceExist iconsResPath "doAllIconsExist"

doAllFontsExist :: IO ()
doAllFontsExist = do
  let fonts = ["Montserrat-VariableFont_wght.ttf"]
  let fontsResPath = ["resources/fonts/" ++ font | font <- fonts]
  doesResourceExist fontsResPath "doAllFontsExist"

doesSettingsFileExist :: IO ()
doesSettingsFileExist = do
  let settingsFile = "directories.txt"
  let settingsResPath = ["resources/data/" ++ settingsFile]
  doesResourceExist settingsResPath "doesSettingsFileExist"
