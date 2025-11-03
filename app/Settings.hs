{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Settings where

import Colors
import Control.Lens
import Data.Text (pack, unpack)
import Graphics.UI.TinyFileDialogs (selectFolderDialog)
import Monomer

data SettingsModel = SettingsModel
  { _searchDirectories :: [String]
  }
  deriving (Eq, Show)

data SettingsEvent
  = SettingsInit [String]
  | SettingsBrowseFolders
  | SettingsAddFolder String
  | SettingsDeleteFolder String
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

fetchInitialFolders :: IO SettingsEvent
fetchInitialFolders = do
  return SettingsNone

browseFoldersHandler :: IO SettingsEvent
browseFoldersHandler = do
  -- TODO: replace with current directory, maybe in the future keep track of previous search
  folder <- selectFolderDialog "Select Wallpaper Folders" "/home/warrenwu/backgrounds"
  case folder of
    Just f -> return $ SettingsAddFolder (unpack f)
    Nothing -> return SettingsNone

buildUISettings :: SettingsEnv -> SettingsModel -> SettingsNode
buildUISettings wenv model = widgetTree
  where
    widgetTree =
      vscroll
        ( vstack_
            [childSpacing_ 24]
            [ box_
                [alignLeft, onClick SettingsBrowseFolders]
                ( label
                    "Add Search Folders"
                    `styleBasic` [paddingH 12, paddingV 8, textColor (rgbHex bg0), bgColor (rgbHex fg1), textFont "SemiBold", textSize 16]
                ),
              vstack_
                [childSpacing_ 4]
                directories
            ]
        )

    directoryText :: String -> SettingsNode
    directoryText dir = label (pack dir) `styleBasic` [textFont "SemiBold", textSize 16, textColor (rgbHex fg1)]

    directories :: [SettingsNode] = map directoryText (model ^. searchDirectories)

handleEventSettings :: SettingsEnv -> SettingsNode -> SettingsModel -> SettingsEvent -> [EventResponse SettingsModel SettingsEvent sp ep]
handleEventSettings wenv node model evt = case evt of
  SettingsInit paths -> [Model $ model & searchDirectories .~ paths]
  SettingsBrowseFolders -> [Task browseFoldersHandler]
  SettingsAddFolder path -> [Model $ model & searchDirectories .~ snoc (model ^. searchDirectories) path]
  SettingsDeleteFolder path -> []
  SettingsNone -> []
