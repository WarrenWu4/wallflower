{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Settings where

import Control.Lens
import LoggerGe
import Monomer
import Graphics.UI.TinyFileDialogs (selectFolderDialog)
import Data.Text (pack)

data SettingsModel = SettingsModel
  { _searchDirectories :: [String]
  }
  deriving (Eq, Show)

data SettingsEvent
  = SettingsInit
  | SettingsBrowseFolders
  | SettingsAddFolder String
  | SettingsNone
  deriving (Eq, Show)

type SettingsEnv = WidgetEnv SettingsModel SettingsEvent

type SettingsNode = WidgetNode SettingsModel SettingsEvent

makeLenses 'SettingsModel

defaultSettingsModel :: SettingsModel
defaultSettingsModel =
  SettingsModel
    { _searchDirectories = []
    }

browseFoldersHandler :: IO SettingsEvent
browseFoldersHandler = do
  logMsg DEBUG "Browsing folders..."
  folder <- selectFolderDialog "Select Wallpaper Folders" "/home/warrenwu/backgrounds"
  case folder of
    Just f -> logMsg DEBUG "Folder selected"
    Nothing -> logMsg DEBUG "No folder selected"
  return SettingsNone

buildUISettings :: SettingsEnv -> SettingsModel -> SettingsNode
buildUISettings wenv model = widgetTree
  where
    widgetTree = 
      vscroll 
        ( vstack_
            [childSpacing_ 12]
            [ 
              box_ [onClick SettingsBrowseFolders] (label "folder"),
              vstack_
                [childSpacing_ 4]
                directories
            ]
        )

    directories :: [SettingsNode] = map (\directory -> label (pack directory)) (model ^. searchDirectories)

handleEventSettings :: SettingsEnv -> SettingsNode -> SettingsModel -> SettingsEvent -> [EventResponse SettingsModel SettingsEvent sp ep]
handleEventSettings wenv node model evt = case evt of
  SettingsInit -> [] 
  SettingsBrowseFolders -> [Task browseFoldersHandler]
  SettingsAddFolder path -> [Model $ model & searchDirectories .~ snoc (model ^. searchDirectories) path]
  SettingsNone -> []


