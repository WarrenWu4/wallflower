{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ResourceLoader
  ( loadUI,
    loadCSS,
    loadFont,
  )
where

import ContentAreaManager
import Control.Monad
import Data.Int
import Data.Text.Internal
import DirectoryManager
import FontBindings
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import LoggerGe
import MarkupInjector
import UiData
import Utilities

placeImages :: Gtk.Grid -> [Gtk.Button] -> IO ()
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

  -- generate action template & build ui file
  actionsRaw <- mapM insertActionTemplate $ zip3 getActionIds getActionIcons getActionLabels
  buildTemplate (concat actionsRaw) "resources/ui/actions.ui"

  -- load action ui file & append to main window
  actionsPath <- getResourcePath "resources/ui/actions.ui"
  Gtk.builderAddFromFile builder actionsPath
  actionButtons <- mapM (getObjectSafe builder Gtk.Button . pack) getActionButtonIds
  actionBar <- Gtk.unsafeCastTo Gtk.Box =<< getObjectSafe builder Gtk.Box "action-bar"
  forM_ actionButtons $ \container -> do Gtk.boxAppend actionBar container

  -- set onclick event handler for each button
  zipWithM_ (applyActions builder) actionButtons getActionIds
  
  -- set pointer cursor on hover
  fallback <- Gtk.widgetGetCursor actionBar
  pointerCursor <- Gdk.cursorNewFromName "pointer" fallback
  forM_ actionButtons $ \btn -> Gtk.widgetSetCursor btn pointerCursor 

  -- set default ui button states
  setButtonState builder "wallpapers" "resources/icons/wallpaper-icon-d.png" True
  setButtonState builder "settings" "resources/icons/settings-icon-l.png" False

  logMsg OK "Action bar loaded"

loadWallpapers :: Gtk.Builder -> IO ()
loadWallpapers builder = do
  logMsg INFO "Loading wallpapers"
  let searchDirectories = getDirectoriesFromSetting ""
  imagePaths <- getImagesInDirectories searchDirectories
  imageMarkups <- zipWithM buildImageTemplate imagePaths [1 .. (length imagePaths)]
  createTempFile $ concat imageMarkups
  imageFiles <- getResourcePath "resources/ui/images.ui"
  wallpaperFile <- getResourcePath "resources/ui/wallpapers.ui"
  Gtk.builderAddFromFile builder imageFiles
  Gtk.builderAddFromFile builder wallpaperFile
  let imageIds = ["btn-background-image-" ++ show n | n <- [1 .. length imageMarkups]]
  imgs <- mapM (getObjectSafe builder Gtk.Button . pack) imageIds
  Just imgContainerObj <- Gtk.builderGetObject builder "wallpaper-container"
  imgContainer <- Gtk.unsafeCastTo Gtk.Grid imgContainerObj
  zipWithM_ (applyBackgroundAction builder) [1 .. length imageMarkups] imagePaths
  placeImages imgContainer imgs
  logMsg OK "Wallpapers loaded"

loadSettings :: Gtk.Builder -> IO ()
loadSettings builder = do
  logMsg INFO "Loading settings"
  settingsFile <- getResourcePath "resources/ui/settings.ui"
  Gtk.builderAddFromFile builder settingsFile
  logMsg OK "Settings loaded"

loadUI :: Gtk.Application -> IO ()
loadUI app = do
  -- tab ids: wallpaper-container, settings-container
  uiFile <- getResourcePath "resources/ui/window.ui"
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
  cssPath <- getResourcePath "resources/css/style.css"
  provider <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromPath provider cssPath
  display <- Gdk.displayGetDefault
  case display of
    Nothing -> putStrLn "Failed to get default display"
    Just d -> do
      Gtk.styleContextAddProviderForDisplay
        d
        provider
        (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)

loadFont :: IO ()
loadFont = do
  fontPath <- getResourcePath "resources/fonts/Montserrat-VariableFont_wght.ttf"
  success <- addFontFile (pack fontPath)
  if success
    then
      logMsg OK $ "Font loaded from: " ++ fontPath
    else logMsg ERROR $ "Failed to load font from: " ++ fontPath
