module HyprpaperManager
  ( applyWallpaper,
    updateWallpaper,
  )
where

import Foreign.C.String
import Foreign.C.Types
import LoggerGe
import System.Process (callCommand)

foreign import ccall "updateWallpapaer"
  c_updateWallpapaer :: CString -> CString -> CInt -> IO ()

-- | runs external C function to save wallpaper in hyprpaper config
-- @monitorName (String): name of the monitor (can be empty string "")
-- @imagePath (String): path to wallpaper image (must be absolute path or relative to home)
-- @mode (Int): wallpaper mode (0: cover, 1: contain, 2: tile)
updateWallpaper :: String -> String -> Int -> IO ()
updateWallpaper monitorName imagePath mode = do
  logMsg DEBUG $ "Updating wallpaper for monitor: " ++ monitorName ++ " with image: " ++ imagePath
  withCString monitorName $ \c_monitorName ->
    withCString imagePath $ \c_imagePath -> do
      let c_mode = fromIntegral mode :: CInt
      c_updateWallpapaer c_monitorName c_imagePath c_mode 
      logMsg DEBUG "Wallpaper update call finished."

-- | runs hyprpaper command to set wallpaper
-- @wallpaperPath (FilePath): path to wallpaper image
applyWallpaper :: FilePath -> IO ()
applyWallpaper wallpaperPath = do
  let display = ","
  let command = "hyprctl hyprpaper reload " ++ display ++ "\"" ++ wallpaperPath ++ "\""
  logMsg INFO $ "applying wallpaper using command: " ++ command
  callCommand command
