{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module ContentAreaManager
  ( applyActions,
    setContentArea,
    clearContentArea,
    setButtonState
  )
where

import Data.Text (pack, Text)
import qualified GI.Gtk as Gtk
import LoggerGe
import Utilities
import UiData

setButtonState :: Gtk.Builder -> String -> String -> Bool -> IO ()
setButtonState builder actionId btnIconPath active = do
  let btnId = getActionButtonId actionId
  let btnLabel = getActionLabelId actionId
  let btnIcon = getActionIconId actionId
  let (addedClass, removedClass) = if active then ("", "-inactive") else ("-inactive", "")
  btnObj <- getObjectSafe builder Gtk.Button (pack btnId) 
  btnLabelObj <- getObjectSafe builder Gtk.Label (pack btnLabel) 
  btnIconObj <- getObjectSafe builder Gtk.Image (pack btnIcon) 
  Gtk.widgetRemoveCssClass btnObj $ pack ("action-btn" ++ removedClass)
  Gtk.widgetRemoveCssClass btnLabelObj $ pack ("action-label" ++ removedClass)
  Gtk.widgetAddCssClass btnObj $ pack ("action-btn" ++ addedClass)
  Gtk.widgetAddCssClass btnLabelObj $ pack ("action-label" ++ addedClass)
  absBtnIconPath <- getResourcePath btnIconPath
  Gtk.imageSetFromFile btnIconObj (Just absBtnIconPath)

applyActions:: Gtk.Builder -> Gtk.Button -> String -> IO ()
applyActions builder btn action = do
  case action of 
    "wallpapers" -> do
      _ <- Gtk.on btn #clicked $ do
        logMsg DEBUG "Wallpapers button clicked"
        setContentArea builder Gtk.Grid "wallpaper-container"
        setButtonState builder "wallpapers" "resources/icons/wallpaper-icon-d.png" True
        setButtonState builder "settings" "resources/icons/settings-icon-l.png" False 
      return ()

    "settings" -> do
      _ <- Gtk.on btn #clicked $ do
        logMsg DEBUG "Settings button clicked"
        setContentArea builder Gtk.Box "settings-container"
        setButtonState builder "settings" "resources/icons/settings-icon-d.png" True 
        setButtonState builder "wallpapers" "resources/icons/wallpaper-icon-l.png" False 
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
