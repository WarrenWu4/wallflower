{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module ContentAreaManager
  ( applyActions,
    setContentArea,
    clearContentArea,
  )
where

import Data.Text (Text)
import qualified GI.Gtk as Gtk
import LoggerGe
import Utilities

applyActions:: Gtk.Builder -> Gtk.Button -> String -> IO ()
applyActions builder btn action = do
  case action of 
    "wallpapers" -> do
      _ <- Gtk.on btn #clicked $ do
        logMsg DEBUG "Wallpapers button clicked"
        setContentArea builder Gtk.Grid "wallpaper-container"
      return ()
    "settings" -> do
      _ <- Gtk.on btn #clicked $ do
        logMsg DEBUG "Settings button clicked"
        setContentArea builder Gtk.Box "settings-container"
      return ()
    _ -> do
      logMsg ERROR $ "Unknown action: " ++ action
      error ""

setContentArea :: (Gtk.GObject o) => Gtk.Builder -> (Gtk.ManagedPtr o -> o) -> Text -> IO ()
setContentArea builder constructor areaId = do
  logMsg INFO $ "Setting content area to " ++ show areaId
  clearContentArea builder
  contentArea <- getObjectSafe builder Gtk.Box "content-area"
  content <- getObjectSafe builder constructor areaId
  widget <- Gtk.unsafeCastTo Gtk.Widget content
  Gtk.boxAppend contentArea widget 
  logMsg OK $ "Content area set to " ++ show areaId

clearContentArea :: Gtk.Builder -> IO ()
clearContentArea builder = do
  logMsg INFO "Clearing content area"
  contentArea <- getObjectSafe builder Gtk.Box "content-area"
  child <- Gtk.widgetGetFirstChild contentArea
  case child of
    Just c -> do
      Gtk.boxRemove contentArea c
    Nothing -> logMsg DEBUG "No child to remove from content area"
  logMsg OK "Content area cleared"
