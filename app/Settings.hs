{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Settings where

import Colors
import Control.Lens
import Data.List (nub)
import Data.Text (pack, unpack)
import DirectoryManager (saveDirectoryToSetting)
import FileParser (parseSettingsFile)
import Graphics.UI.TinyFileDialogs (selectFolderDialog)
import Monomer
import Utilities

data SettingsModel = SettingsModel
  { _directories :: [String]
  }
  deriving (Eq, Show)

data SettingsEvent
  = SettingsInit
  | SettingsBrowseFolders
  | SettingsAddFolders [String]
  | SettingsAddFolder String
  | SettingsDeleteFolder String
  | SettingsNothing
  deriving (Eq, Show)

type SettingsEnv = WidgetEnv SettingsModel SettingsEvent

type SettingsNode = WidgetNode SettingsModel SettingsEvent

makeLenses 'SettingsModel

defaultSettingsModel :: SettingsModel
defaultSettingsModel =
  SettingsModel
    { _directories = []
    }

fetchInitialFolders :: IO SettingsEvent
fetchInitialFolders = do 
  settingsFile <- getSettingsFile
  settingsDirectories <- readSettings settingsFile
  return $ SettingsAddFolders settingsDirectories 

addDirectoryToFile :: String -> IO SettingsEvent
addDirectoryToFile dir = do
  settingsFile <- getSettingsFile
  settingsDirectories <- readSettings settingsFile
  let appendedDirs = snoc settingsDirectories dir
  let uniqueDirs = nub appendedDirs
  writeSettings settingsFile uniqueDirs
  return SettingsNothing

removeDirectoryFromFile :: String -> IO SettingsEvent
removeDirectoryFromFile dir = do
  settingsFile <- getSettingsFile
  settingsDirectories <- readSettings settingsFile
  let updatedDirs = filter (/= dir) settingsDirectories
  writeSettings settingsFile updatedDirs
  return SettingsNothing

browseFoldersHandler :: IO SettingsEvent
browseFoldersHandler = do
  -- TODO: replace with current directory, maybe in the future keep track of previous search
  folder <- selectFolderDialog "Select Wallpaper Folders" "/home/warrenwu/backgrounds"
  case folder of
    Just f -> return $ SettingsAddFolder (unpack f)
    Nothing -> return SettingsNothing

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
                [childSpacing_ 12]
                directoryItems
            ]
        )

    directoryText :: String -> SettingsNode
    directoryText dir =
      box_
        [alignLeft, onClick $ SettingsDeleteFolder dir]
        ( hstack_
            [childSpacing_ 8]
            [ image_
                (pack "./resources/icons/folder-icon.png")
                [fitWidth]
                `styleBasic` [width 16, height 16],
              label (pack dir) `styleBasic` [textFont "SemiBold", textSize 16, textColor (rgbHex fg1)]
            ]
        )

    directoryItems :: [SettingsNode] = map directoryText (model ^. directories)

handleEventSettings :: SettingsEnv -> SettingsNode -> SettingsModel -> SettingsEvent -> [EventResponse SettingsModel SettingsEvent sp ep]
handleEventSettings wenv node model evt = case evt of
  SettingsInit -> [Task fetchInitialFolders]
  SettingsBrowseFolders -> [Task browseFoldersHandler]
  SettingsAddFolders paths -> [Model $ model & directories .~ paths]
  SettingsAddFolder path ->
    [ Task $ addDirectoryToFile path,
      Model $
        model
          & directories %~ \dirs ->
            let appendedDirs = snoc dirs path
             in nub appendedDirs
    ]
  SettingsDeleteFolder path ->
    [ Task $ removeDirectoryFromFile path,
      Model $
        model
          & directories %~ \dirs -> filter (/= path) dirs
    ]
  SettingsNothing -> []
