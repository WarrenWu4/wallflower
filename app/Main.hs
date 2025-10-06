{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void, zipWithM)
import Data.GI.Base
import DirectoryManager
import qualified GI.Gtk as Gtk
import MarkupInjector
import ResourceLoader
import Validator (checkHyprland, checkHyprpaper)

main :: IO ()
main = void $ do
  checkHyprland
  checkHyprpaper

  app <- new Gtk.Application [#applicationId := "com.warrenwu.wallflower"]

  let searchDirectories = getDirectoriesFromSetting ""
  imagePaths <- getImagesInDirectories searchDirectories
  imageMarkups <- zipWithM createImageMarkup imagePaths [1 .. (length imagePaths)]

  _ <- on app #activate $ do
    loadCSS
    loadUI app imageMarkups

  _ <- #run app Nothing
  return ()
