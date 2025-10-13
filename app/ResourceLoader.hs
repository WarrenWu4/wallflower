{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ResourceLoader
  ( loadResources,
    loadFont,
    loadCSS,
    loadWindow,
    displayWindow,
  )
where

import ContentAreaManager
import Control.Monad
import Data.Int
import Data.Text.Internal
import FontBindings
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import LoggerGe
import TemplateBuilder
import UiData
import Utilities
import DirectoryManager (saveDirectoryToSetting)
import Data.Char (ord)

placeImages :: Gtk.Grid -> [Gtk.Button] -> IO ()
placeImages container imgs = do
  let indexedImgs = zip [0 .. (length imgs - 1)] imgs
  forM_ indexedImgs $ \(i, img) -> do
    let num = fromIntegral i :: Int32
    let col = num `mod` 3 :: Int32
    let row = num `div` 3 :: Int32
    Gtk.gridAttach container img col row 1 1

-- loadWallpapers :: Gtk.Builder -> IO ()
-- loadWallpapers builder = do
--   logMsg INFO "Loading wallpapers"
--   searchDirectories <- getDirectoriesFromSetting
--   imagePaths <- getImagesInDirectories searchDirectories
--   imageMarkups <- zipWithM buildImageTemplate imagePaths [1 .. (length imagePaths)]
--   createTempFile $ concat imageMarkups
--   imageFiles <- getResourcePath "resources/ui/images.ui"
--   wallpaperFile <- getResourcePath "resources/ui/wallpapers.ui"
--   Gtk.builderAddFromFile builder imageFiles
--   Gtk.builderAddFromFile builder wallpaperFile
--   let imageIds = ["btn-background-image-" ++ show n | n <- [1 .. length imageMarkups]]
--   imgs <- mapM (getObjectSafe builder Gtk.Button . pack) imageIds
--   Just imgContainerObj <- Gtk.builderGetObject builder "wallpaper-container"
--   imgContainer <- Gtk.unsafeCastTo Gtk.Grid imgContainerObj
--   zipWithM_ (applyBackgroundAction builder) [1 .. length imageMarkups] imagePaths
--   placeImages imgContainer imgs
--   logMsg OK "Wallpapers loaded"

-- | wrapper function that loads, builds, and applies all resources
loadResources :: Gtk.Application -> IO ()
loadResources app = do
  loadFont
  loadCSS
  builder <- Gtk.builderNew
  loadWindow builder
  loadUiFiles builder
  displayWindow app builder

loadFont :: IO ()
loadFont = do
  fontPath <- getResourcePath "resources/fonts/Montserrat-VariableFont_wght.ttf"
  success <- addFontFile (pack fontPath)
  if success
    then
      logMsg OK $ "Font loaded from: " ++ fontPath
    else logMsg ERROR $ "Failed to load font from: " ++ fontPath

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

loadWindow :: Gtk.Builder -> IO ()
loadWindow builder = do
  uiFile <- getResourcePath "resources/ui/window.ui"
  _ <- Gtk.builderAddFromFile builder uiFile
  return ()

loadUiFiles:: Gtk.Builder -> IO ()
loadUiFiles builder = do
  -- load tab ui files 
  tabFile <- getResourcePath "resources/ui/tab.ui"
  tabContent <- generateTabMarkup
  buildTemplate tabContent tabFile 
  _ <- Gtk.builderAddFromFile builder tabFile
  -- additional processing for states and event handlers
  -- set pointer cursor on hover
  -- add tabs to main window
  tabParent <- getObjectSafe builder Gtk.Box "action-bar"
  fallback <- Gtk.widgetGetCursor tabParent 
  pointerCursor <- Gdk.cursorNewFromName "pointer" fallback
  forM_ getTabData $ \(tabId, tabIcon, tabIconId, _, tabLabelId) -> do
    iconPath <- getResourcePath $ "resources/icons/" ++ tabIcon
    if tabId == "tab-wallpapers"
      then do
        setButtonState builder tabId iconPath tabIconId tabLabelId True
      else do
        setButtonState builder tabId iconPath tabIconId tabLabelId False
    tabObj <- getObjectSafe builder Gtk.Button (pack tabId)
    applyActions builder tabObj tabId
    Gtk.widgetSetCursor tabObj pointerCursor
    Gtk.boxAppend tabParent tabObj

  -- load wallpapers ui file
  wallpaperFile <- getResourcePath "resources/ui/images.ui"
  wallpaperContent <- generateWallpaperMarkup 
  buildTemplate wallpaperContent wallpaperFile 
  _ <- Gtk.builderAddFromFile builder wallpaperFile 
  wallpaperParentFile <- getResourcePath "resources/ui/wallpapers.ui"
  _ <- Gtk.builderAddFromFile builder wallpaperParentFile 
  -- add wallpapers to parent container 
  -- TODO: add event handler and active styling
  wallpaperData <- getWallpaperData
  wallpaperParent <- getObjectSafe builder Gtk.Box "wallpaper-container"
  wallpaperCol1 <- getObjectSafe builder Gtk.Box "wallpaper-col-1"
  wallpaperCol2 <- getObjectSafe builder Gtk.Box "wallpaper-col-2"
  wallpaperCol3 <- getObjectSafe builder Gtk.Box "wallpaper-col-3"
  forM_ wallpaperData $ \(imgId, imgBtnId, _) -> do
    let number = ord $ last imgId 
    btnObj <- getObjectSafe builder Gtk.Button (pack imgBtnId)
    if number `mod` 3 == 1
      then Gtk.boxAppend wallpaperCol1 btnObj
      else if number `mod` 3 == 2
        then Gtk.boxAppend wallpaperCol2 btnObj
        else Gtk.boxAppend wallpaperCol3 btnObj
  -- add parent container to main window
  contentArea <- getObjectSafe builder Gtk.Box "content-area"
  Gtk.boxAppend contentArea wallpaperParent

  -- load settings ui file
  dirFile <- getResourcePath "resources/ui/directories.ui"
  dirContent <- generateDirectoryListMarkup
  buildTemplate dirContent dirFile
  _ <- Gtk.builderAddFromFile builder dirFile
  dirParentFile <- getResourcePath "resources/ui/settings.ui"
  _ <- Gtk.builderAddFromFile builder dirParentFile 
  -- add directory list to parent container
  dirData <- getDirectoryData
  dirParent <- getObjectSafe builder Gtk.Box "settings-directory-container"
  dirs <- mapM (getObjectSafe builder Gtk.Box . pack) ["directory-" ++ show n | n <- [1 .. length dirData]]
  forM_ dirs (\dir -> do Gtk.boxAppend dirParent dir)
  -- add onclick event handler
  logMsg DEBUG "Setting up add directory button"
  browseBtn <- getObjectSafe builder Gtk.Button "settings-browse-btn"
  _ <- Gtk.on browseBtn #clicked $ do
    logMsg DEBUG "Browse button clicked"
    fileDialog <- Gtk.fileDialogNew
    Gtk.fileDialogSetTitle fileDialog "Select Folder"
    Gtk.fileDialogSelectFolder
      fileDialog
      (Nothing :: Maybe Gtk.Window)
      (Nothing :: Maybe Gio.Cancellable)
      $ Just
        ( \_ r -> do
            result <- Gtk.fileDialogSelectFolderFinish fileDialog r
            Just folder <- Gio.fileGetPath result
            saveDirectoryToSetting folder
        )
  return ()

displayWindow :: Gtk.Application -> Gtk.Builder -> IO ()
displayWindow app builder = do
  windowObj <- getObjectSafe builder Gtk.ApplicationWindow "main_window"
  #setApplication windowObj (Just app)
  Gtk.setWidgetVisible windowObj True
