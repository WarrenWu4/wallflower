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
    loadUI app

  _ <- #run app Nothing
  pure ()
