{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Settings where

import Control.Lens
import Monomer

data SettingsModel = SettingsModel
  { _searchDirectories :: [String]
  }
  deriving (Eq, Show)

data SettingsEvent
  = SettingsInit
  deriving (Eq, Show)

type SettingsEnv = WidgetEnv SettingsModel SettingsEvent

type SettingsNode = WidgetNode SettingsModel SettingsEvent

makeLenses 'SettingsModel

defaultSettingsModel :: SettingsModel
defaultSettingsModel =
  SettingsModel
    { _searchDirectories = []
    }

buildUISettings :: SettingsEnv -> SettingsModel -> SettingsNode
buildUISettings wenv model = widgetTree
  where
    widgetTree = vstack [label "Settings"]

handleEventSettings :: SettingsEnv -> SettingsNode -> SettingsModel -> SettingsEvent -> [EventResponse SettingsModel SettingsEvent sp ep]
handleEventSettings wenv node model evt = case evt of
  SettingsInit -> [] 


