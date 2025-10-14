module HyprpaperManager
  ( applyWallpaper,
    saveWallpaper,
  )
where

import LoggerGe
import System.Directory (getHomeDirectory)
import System.Process (callCommand)

-- | runs hyprpaper command to set wallpaper
-- @wallpaperPath (FilePath): path to wallpaper image
applyWallpaper :: FilePath -> IO ()
applyWallpaper wallpaperPath = do
  let display = ","
  let command = "hyprctl hyprpaper reload " ++ display ++ "\"" ++ wallpaperPath ++ "\""
  logMsg INFO $ "applying wallpaper using command: " ++ command
  callCommand command

-- | updates hyrppaper config file with new wallpaper
-- @wallpaperPath (FilePath): path to wallpaper image
saveWallpaper :: FilePath -> IO ()
saveWallpaper wallpaperPath = do
  let monitorName = ","
  homeDir <- getHomeDirectory
  let configPath = homeDir ++ "/.config/hypr/hyprpaper.conf"
  let monitorTarget = if null monitorName then "" else monitorName
  let configContent =
        "preload = "
          ++ wallpaperPath
          ++ "\n"
          ++ "wallpaper = "
          ++ monitorTarget
          ++ ","
          ++ wallpaperPath
          ++ "\n"
          ++ "ipc = true\n" -- Ensure IPC is enabled for runtime changes
  putStrLn $ "Writing persistent configuration to: " ++ configPath
  writeFile configPath configContent
  putStrLn "Configuration saved. The new wallpaper will persist after reboot."
