module EventHandlers (switchWallpaper) where

import HyprpaperManager (applyWallpaper, saveWallpaper)
import qualified GI.Gtk as Gtk
import UiData (getWallpaperData)
import Control.Monad (forM_)
import Utilities (setWallpaperStyle)

switchWallpaper :: Gtk.Builder -> FilePath -> IO ()
switchWallpaper builder imgPath = do
  applyWallpaper imgPath
  -- FIX: not safe
  -- saveWallpaper imgPath
  wallpaperData <- getWallpaperData
  forM_ wallpaperData $ \(imgId, _, path) -> do
    if path == imgPath
      then setWallpaperStyle builder imgId True
      else setWallpaperStyle builder imgId False





