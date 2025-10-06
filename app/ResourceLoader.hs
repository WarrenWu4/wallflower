{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module ResourceLoader
  ( loadResource,
    getResourcePath,
    loadUI,
    loadCSS,
  )
where

import Control.Monad (forM_)
import Data.Int
import Data.Text.Internal
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import LoggerGe
import MarkupInjector
import Paths_wallflower (getDataFileName)

loadResource :: FilePath -> IO String
loadResource path = do
  absPath <- getDataFileName path
  readFile absPath

getResourcePath :: FilePath -> IO FilePath
getResourcePath path = do
  getDataFileName path

getImageById :: Gtk.Builder -> Text -> IO Gtk.Picture
getImageById builder objectId = do
  Just obj <- Gtk.builderGetObject builder objectId
  Gtk.unsafeCastTo Gtk.Picture obj

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
            Just obj <- Gtk.builderGetObject b id
            Gtk.unsafeCastTo Gtk.Box obj
        )
  actionObjs <- mapM (getActionById builder . pack) ["btn-" ++ a ++ "-container" | a <- actions]

  Just actionBarObj <- Gtk.builderGetObject builder "action-bar"
  actionBar <- Gtk.unsafeCastTo Gtk.Box actionBarObj
  _ <- mapM (\a -> Gtk.boxAppend actionBar a) actionObjs
  logMsg OK "Action bar loaded"

loadUI :: Gtk.Application -> [String] -> IO ()
loadUI app imageMarkups = do
  createTempFile $ concat imageMarkups
  imageFiles <- getResourcePath "resources/images.ui"
  popupFile <- getResourcePath "resources/settings.ui"

  uiFile <- getResourcePath "resources/window.ui"
  builder <- Gtk.builderNew
  _ <- Gtk.builderAddFromFile builder uiFile
  _ <- Gtk.builderAddFromFile builder imageFiles
  _ <- Gtk.builderAddFromFile builder popupFile

  loadActionBar builder

  Just winObj <- Gtk.builderGetObject builder "main_window"
  window <- Gtk.unsafeCastTo Gtk.ApplicationWindow winObj

  Just popupObj <- Gtk.builderGetObject builder "settings_window"
  popup <- Gtk.unsafeCastTo Gtk.Window popupObj
  Just closeBtnOj <- Gtk.builderGetObject builder "close_window"
  closeBtn <- Gtk.unsafeCastTo Gtk.Button closeBtnOj

  _ <- Gtk.on closeBtn #clicked $ do
    logMsg INFO "Closing settings window"
    Gtk.setWidgetVisible popup False

  Gtk.setWindowTransientFor popup window

  let imageIds = ["background-image-" ++ show n | n <- [1 .. length imageMarkups]]
  imgs <- mapM (getImageById builder . pack) imageIds

  Just imgContainerObj <- Gtk.builderGetObject builder "backgrounds"
  imgContainer <- Gtk.unsafeCastTo Gtk.Grid imgContainerObj

  placeImages imgContainer imgs

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
