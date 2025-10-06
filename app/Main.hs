{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import Data.GI.Base
import qualified GI.Gtk as Gtk
import ResourceLoader
import Validator (checkHyprland, checkHyprpaper)
import LoggerGe

main :: IO ()
main = void $ do
  logMsg INFO "Starting application"
  checkHyprland
  checkHyprpaper

  app <- new Gtk.Application [#applicationId := "com.warrenwu.wallflower"]


  _ <- on app #activate $ do
    loadCSS
    loadUI app

  _ <- #run app Nothing
  return ()
