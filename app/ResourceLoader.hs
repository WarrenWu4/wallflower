{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ResourceLoader
  ( loadResource,
    getResourcePath,
    loadUI,
    loadCSS,
    loadFont
  )
where

import ContentAreaManager
import Control.Monad (forM_, zipWithM, zipWithM_)
import Data.Int
import Data.Text.Internal
import DirectoryManager
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import LoggerGe
import MarkupInjector
import Paths_wallflower (getDataFileName)
import Utilities
import FontBindings

loadResource :: FilePath -> IO String
loadResource path = do
  absPath <- getDataFileName path
  readFile absPath

getResourcePath :: FilePath -> IO FilePath
getResourcePath path = do
  getDataFileName path

placeImages :: Gtk.Grid -> [Gtk.Button] -> IO ()
placeImages container imgs = do
  let indexedImgs = zip [0 .. (length imgs - 1)] imgs
  forM_ indexedImgs $ \(i, img) -> do
    let num = fromIntegral i :: Int32
    let col = num `mod` 3 :: Int32
    let row = num `div` 3 :: Int32
    Gtk.gridAttach container img col row 1 1

loadActionIcon :: Gtk.Builder -> String -> String -> IO ()
loadActionIcon builder action iconPath = do
  container <- getObjectSafe builder Gtk.Box (pack $ "btn-" ++ action ++ "-container")
  box <- Gtk.unsafeCastTo Gtk.Box container 
  imgObj <- getResourcePath iconPath
  img <- Gtk.imageNewFromFile imgObj
  Gtk.boxPrepend box img 

loadActionBar :: Gtk.Builder -> IO ()
loadActionBar builder = do
  logMsg INFO "Loading action bar"
  let actions = ["wallpapers", "settings"]
  let icons = ["resources/wallpaper-icon-d.png", "resources/settings-icon-l.png"]
  actionBarFile <- getResourcePath "resources/actions.ui"
  Gtk.builderAddFromFile builder actionBarFile
  let getActionById b actionId = do getObjectSafe b Gtk.Box actionId
  actionObjs <- mapM (getActionById builder . pack) ["btn-" ++ a ++ "-container" | a <- actions]
  Just actionBarObj <- Gtk.builderGetObject builder "action-bar"
  actionBar <- Gtk.unsafeCastTo Gtk.Box actionBarObj
  mapM_ (Gtk.boxAppend actionBar) actionObjs
  actionButtons <- mapM (getObjectSafe builder Gtk.Button . pack) ["btn-" ++ a | a <- actions]
  zipWithM_ (applyActions builder) actionButtons actions
  zipWithM_ (loadActionIcon builder) actions icons
  logMsg OK "Action bar loaded"

loadWallpapers :: Gtk.Builder -> IO ()
loadWallpapers builder = do
  logMsg INFO "Loading wallpapers"
  let searchDirectories = getDirectoriesFromSetting ""
  imagePaths <- getImagesInDirectories searchDirectories
  imageMarkups <- zipWithM buildImageTemplate imagePaths [1 .. (length imagePaths)]
  createTempFile $ concat imageMarkups
  imageFiles <- getResourcePath "resources/images.ui"
  wallpaperFile <- getResourcePath "resources/wallpapers.ui"
  Gtk.builderAddFromFile builder imageFiles
  Gtk.builderAddFromFile builder wallpaperFile
  let imageIds = ["btn-background-image-" ++ show n | n <- [1 .. length imageMarkups]]
  imgs <- mapM (getObjectSafe builder Gtk.Button . pack) imageIds
  Just imgContainerObj <- Gtk.builderGetObject builder "wallpaper-container"
  imgContainer <- Gtk.unsafeCastTo Gtk.Grid imgContainerObj
  _ <- zipWithM (applyBackgroundAction builder) [1 .. length imageMarkups] imagePaths
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
        (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)

loadFont :: IO ()
loadFont = do
  fontPath <- getResourcePath "resources/Montserrat-VariableFont_wght.ttf"
  success <- addFontFile (pack fontPath)
  if success then
    logMsg OK $ "Font loaded from: " ++ fontPath
    else logMsg ERROR $ "Failed to load font from: " ++ fontPath
