{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module ResourceLoader
  ( loadResource,
    getResourcePath,
    loadUI,
    loadCSS,
  )
where

import Data.GI.Base
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import MarkupInjector
import Paths_wallflower (getDataFileName)
import System.IO (readFile)
import Data.Text.Internal

loadResource :: FilePath -> IO String
loadResource path = do
  absPath <- getDataFileName path
  contents <- readFile absPath
  return contents

getResourcePath :: FilePath -> IO FilePath
getResourcePath path = do
  absPath <- getDataFileName path
  return absPath

getImageById :: Gtk.Builder -> Text -> IO Gtk.Picture
getImageById builder objectId = do
  Just obj <- Gtk.builderGetObject builder objectId 
  Gtk.unsafeCastTo Gtk.Picture obj

loadUI :: Gtk.Application -> [String] -> IO ()
loadUI app imageMarkups = do
  createTempFile $ concat imageMarkups
  imageFiles <- getResourcePath "resources/images.ui"

  uiFile <- getResourcePath "resources/window.ui"
  builder <- Gtk.builderNew
  _ <- Gtk.builderAddFromFile builder uiFile
  _ <- Gtk.builderAddFromFile builder imageFiles

  Just winObj <- Gtk.builderGetObject builder "main_window"
  window <- Gtk.unsafeCastTo Gtk.ApplicationWindow winObj

  let imageIds = ["background-image-"++ show n | n <- [1..length imageMarkups]]
  imgs <- mapM (getImageById builder . pack) imageIds

  Just imgContainerObj <- Gtk.builderGetObject builder "backgrounds"
  imgContainer <- Gtk.unsafeCastTo Gtk.Box imgContainerObj

  mapM_ (Gtk.boxAppend imgContainer) imgs

  #setApplication window (Just app)

  #show window

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
