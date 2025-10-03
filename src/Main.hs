{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import Control.Monad (void)
import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import Data.GI.Base

main :: IO ()
main = void $ do
  app <- new Gtk.Application [ #applicationId := "com.warrenwu.wallflower" ]
  _ <- on app #activate $ do
    window <- new Gtk.ApplicationWindow [ #application := app
                                        , #title := "Wallflower"
                                        , #defaultWidth := 800
                                        , #defaultHeight := 600
                                        ]
    label <- new Gtk.Label [ #label := "Testing" ]
    #setChild window (Just label)
    #present window
  Gio.applicationRun app Nothing
