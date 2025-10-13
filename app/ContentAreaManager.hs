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

setButtonState :: Gtk.Builder -> String -> FilePath -> String -> String -> Bool -> IO ()
setButtonState builder btnId btnIconPath btnIconId btnLabelId active = do
  let (addedClass, removedClass) = if active then ("", "-inactive") else ("-inactive", "")
  btnObj <- getObjectSafe builder Gtk.Button (pack btnId) 
  btnLabelObj <- getObjectSafe builder Gtk.Label (pack btnLabelId) 
  btnIconObj <- getObjectSafe builder Gtk.Image (pack btnIconId) 
  Gtk.widgetRemoveCssClass btnObj $ pack ("action-btn" ++ removedClass)
  Gtk.widgetRemoveCssClass btnLabelObj $ pack ("action-label" ++ removedClass)
  Gtk.widgetAddCssClass btnObj $ pack ("action-btn" ++ addedClass)
  Gtk.widgetAddCssClass btnLabelObj $ pack ("action-label" ++ addedClass)
  Gtk.imageSetFromFile btnIconObj (Just btnIconPath)

applyActions:: Gtk.Builder -> Gtk.Button -> String -> IO ()
applyActions builder btn action = do
  case action of 
    "tab-wallpapers" -> do
      _ <- Gtk.on btn #clicked $ do
        logMsg DEBUG "Wallpapers button clicked"
        setContentArea builder Gtk.Grid "wallpaper-container"
        iconPath <- getResourcePath "resources/icons/wallpaper-icon-d.png"
        setButtonState builder "tab-wallpapers" iconPath "tab-icon-wallpapers" "tab-label-wallpapers" True
        iconPath2 <- getResourcePath "resources/icons/settings-icon-l.png"
        setButtonState builder "tab-settings" iconPath2 "tab-icon-settings" "tab-label-settings" False 
      return ()

    "tab-settings" -> do
      _ <- Gtk.on btn #clicked $ do
        logMsg DEBUG "Settings button clicked"
        setContentArea builder Gtk.Box "settings-container"
        iconPath <- getResourcePath "resources/icons/wallpaper-icon-l.png"
        setButtonState builder "tab-wallpapers" iconPath "tab-icon-wallpapers" "tab-label-wallpapers" False 
        iconPath2 <- getResourcePath "resources/icons/settings-icon-d.png"
        setButtonState builder "tab-settings" iconPath2 "tab-icon-settings" "tab-label-settings" True 
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
