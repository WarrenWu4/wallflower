{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import Control.Monad (void)
import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import Data.GI.Base

import DirectoryManager
import ResourceLoader

main :: IO ()
main = void $ do
  app <- new Gtk.Application [ #applicationId := "com.warrenwu.wallflower" ]

  let searchDirectories = getDirectoriesFromSetting ""
  imagePaths <- getImagesInDirectories searchDirectories 

  on app #activate $ do
    uiFile <- getResourcePath "resources/window.ui"
    builder <- Gtk.builderNew
    _ <- Gtk.builderAddFromFile builder uiFile

    Just winObj <- Gtk.builderGetObject builder "main_window"
    window <- Gtk.unsafeCastTo Gtk.ApplicationWindow winObj

    #setApplication window (Just app)

    Just btnObj <- Gtk.builderGetObject builder "my_button"
    button <- Gtk.unsafeCastTo Gtk.Button btnObj
    _ <- on button #clicked (putStrLn "Button clicked!")

    #show window

  _ <- #run app Nothing
  pure ()
