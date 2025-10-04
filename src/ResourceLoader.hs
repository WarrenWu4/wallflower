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
  uiPath <- getDataFileName "resources/window.ui"
  return ()
  -- builder <- new Gtk.Builder []
  -- Gtk.builderAddFromFile builder uiPath
  -- Just windowObj <- Gtk.builderGetObject builder "main_window"
  -- window <- Gtk.unsafeCastTo Gtk.Window windowObj
  --
  -- Just buttonObj <- Gtk.builderGetObject builder "my_button"
  -- button <- Gtk.unsafeCastTo Gtk.Button buttonObj
  -- _ <- on button #clicked (putStrLn "Button was clicked!")
  --
  -- #show window
  --
  -- Gtk.main

loadCSS :: IO ()
loadCSS = do
    cssPath <- getDataFileName "resources/style.css"
    return ()
    -- provider <- new Gtk.CssProvider []
    -- #loadFromPath provider cssPath
    -- display <- Gdk.displayGetDefault >>= \case
    --     Just disp -> return disp
    --     Nothing   -> error "Error: Could not get default display."
    -- Gtk.styleContextAddProviderForDisplay display provider
    --     (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)
