{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import Data.GI.Base
import DirectoryManager
import qualified GI.Gtk as Gtk
import ResourceLoader
import MarkupInjector

main :: IO ()
main = void $ do
  app <- new Gtk.Application [#applicationId := "com.warrenwu.wallflower"]

  let searchDirectories = getDirectoriesFromSetting ""
  imagePaths <- getImagesInDirectories searchDirectories
  let imageMarkups = zipWith createImageMarkup imagePaths [1..(length imagePaths)]

  _ <- on app #activate $ do
    loadCSS
    loadUI app imageMarkups 

  _ <- #run app Nothing
  pure ()
