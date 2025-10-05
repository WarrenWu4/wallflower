{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module ResourceLoader (
  loadResource,
  getResourcePath,
  loadUI,
  loadCSS
) where

import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import qualified GI.Gdk as Gdk
import Data.GI.Base
import System.IO (readFile)
import Paths_wallflower (getDataFileName)

loadResource :: FilePath -> IO String
loadResource path = do
  absPath <- getDataFileName path 
  contents <- readFile absPath
  return contents

getResourcePath :: FilePath -> IO FilePath
getResourcePath path = do 
  absPath <- getDataFileName path
  return absPath 

loadUI :: Gtk.Application -> IO ()
loadUI app = do 
  imageFiles <- getResourcePath "resources/images.ui"
  uiFile <- getResourcePath "resources/window.ui"
  builder <- Gtk.builderNew
  _ <- Gtk.builderAddFromFile builder uiFile

  Just winObj <- Gtk.builderGetObject builder "main_window"
  window <- Gtk.unsafeCastTo Gtk.ApplicationWindow winObj

  Just imgContainerObj <- Gtk.builderGetObject builder "backgrounds"
  imgContainer <- Gtk.unsafeCastTo Gtk.Grid imgContainerObj

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
      Just d  -> do
        Gtk.styleContextAddProviderForDisplay 
          d
          provider 
          (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)
