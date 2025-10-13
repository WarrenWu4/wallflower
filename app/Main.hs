{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import Data.GI.Base
import qualified GI.Gtk as Gtk
import ResourceLoader
import Validator (checkAllDependencies)

main :: IO ()
main = void $ do
  checkAllDependencies

  app <- new Gtk.Application [#applicationId := "com.warrenwu.wallflower"]

  _ <- on app #activate $ do
    loadResources app

  _ <- #run app Nothing
  return ()
