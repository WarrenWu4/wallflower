{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ResourceLoader
  ( loadResource,
    getResourcePath,
    loadUI,
    loadCSS,
  )
where

import ContentAreaManager
import Control.Monad (forM_, zipWithM)
import Data.Int
import Data.Text.Internal
import DirectoryManager
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import LoggerGe
import MarkupInjector
import Paths_wallflower (getDataFileName)
import Utilities

loadResource :: FilePath -> IO String
loadResource path = do
  absPath <- getDataFileName path
  readFile absPath

getResourcePath :: FilePath -> IO FilePath
getResourcePath path = do
  getDataFileName path

getImageById :: Gtk.Builder -> Text -> IO Gtk.Picture
getImageById builder objectId = do
  getObjectSafe builder Gtk.Picture objectId

placeImages :: Gtk.Grid -> [Gtk.Picture] -> IO ()
placeImages container imgs = do
  let indexedImgs = zip [0 .. (length imgs - 1)] imgs
  forM_ indexedImgs $ \(i, img) -> do
    let num = fromIntegral i :: Int32
    let col = num `mod` 3 :: Int32
    let row = num `div` 3 :: Int32
    Gtk.gridAttach container img col row 1 1

loadActionBar :: Gtk.Builder -> IO ()
loadActionBar builder = do
  logMsg INFO "Loading action bar"
  let actions = ["wallpapers", "settings"]
  actionBarFile <- getResourcePath "resources/actions.ui"
  Gtk.builderAddFromFile builder actionBarFile
  let getActionById =
        ( \b id -> do
            getObjectSafe b Gtk.Box id
        )
  actionObjs <- mapM (getActionById builder . pack) ["btn-" ++ a ++ "-container" | a <- actions]
  Just actionBarObj <- Gtk.builderGetObject builder "action-bar"
  actionBar <- Gtk.unsafeCastTo Gtk.Box actionBarObj
  _ <- mapM (\a -> Gtk.boxAppend actionBar a) actionObjs
  actionButtons <- mapM (getObjectSafe builder Gtk.Button . pack) ["btn-" ++ a | a <- actions]
  _ <- zipWithM (applyActions builder) actionButtons actions
  logMsg OK "Action bar loaded"

loadWallpapers :: Gtk.Builder -> IO ()
loadWallpapers builder = do
  logMsg INFO "Loading wallpapers"
  let searchDirectories = getDirectoriesFromSetting ""
  imagePaths <- getImagesInDirectories searchDirectories
  imageMarkups <- zipWithM createImageMarkup imagePaths [1 .. (length imagePaths)]
  createTempFile $ concat imageMarkups
  imageFiles <- getResourcePath "resources/images.ui"
  wallpaperFile <- getResourcePath "resources/wallpapers.ui"
  Gtk.builderAddFromFile builder imageFiles
  Gtk.builderAddFromFile builder wallpaperFile
  let imageIds = ["background-image-" ++ show n | n <- [1 .. length imageMarkups]]
  imgs <- mapM (getImageById builder . pack) imageIds
  Just imgContainerObj <- Gtk.builderGetObject builder "wallpaper-container"
  imgContainer <- Gtk.unsafeCastTo Gtk.Grid imgContainerObj
  placeImages imgContainer imgs
  logMsg OK "Wallpapers loaded"

loadSettings :: Gtk.Builder -> IO ()
loadSettings builder = do
  logMsg INFO "Loading settings"
  settingsFile <- getResourcePath "resources/settings.ui"
  Gtk.builderAddFromFile builder settingsFile
  logMsg OK "Settings loaded"

loadUI :: Gtk.Application -> IO ()
loadUI app = do
  -- tab ids: wallpaper-container, settings-container
  uiFile <- getResourcePath "resources/window.ui"
  builder <- Gtk.builderNew
  _ <- Gtk.builderAddFromFile builder uiFile

  loadActionBar builder

  loadWallpapers builder
  loadSettings builder

  setContentArea builder Gtk.Grid "wallpaper-container"

  Just winObj <- Gtk.builderGetObject builder "main_window"
  window <- Gtk.unsafeCastTo Gtk.ApplicationWindow winObj

  #setApplication window (Just app)
  Gtk.setWidgetVisible window True

loadCSS :: IO ()
loadCSS = do
  cssPath <- getResourcePath "resources/style.css"
  provider <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromPath provider cssPath
  display <- Gdk.displayGetDefault
  case display of
    Nothing -> putStrLn "Failed to get default display"
    Just d -> do
      Gtk.styleContextAddProviderForDisplay
        d
        provider
        (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)
