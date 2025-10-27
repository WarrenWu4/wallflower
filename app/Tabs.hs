{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Tabs where

import Monomer
import Control.Lens
import Data.Text (pack)
import Colors

data TabModel = TabModel {
    _tabs :: [(String, String)],
    _tabActive :: String
  } deriving (Eq, Show)

data TabEvent
  = TabOnClick String
  | TabSetActive String
  deriving (Eq, Show)

type TabEnv = WidgetEnv TabModel TabEvent

type TabNode = WidgetNode TabModel TabEvent

makeLenses 'TabModel

defaultTabModel :: TabModel
defaultTabModel = TabModel {
    _tabs = getAllTabs,
    _tabActive = "Wallpapers"
  }

buildUITab :: TabEnv -> TabModel -> TabNode
buildUITab wenv model = widgetTree
  where
    widgetTree = hstack_ [childSpacing_ 16] tabUIs

    tabUIs =
      map
        ( \(tabName, tabIcon) ->
            if tabName == (model ^. tabActive)
              then
                hstack_
                  [childSpacing_ 8]
                  [ image (pack $ tabIcon ++ "-d.png")
                      `styleBasic` [width 16, height 16],
                    label (pack tabName)
                      `styleBasic` [textFont "SemiBold", textSize 16, textColor (rgbHex bg0)]
                  ]
                  `styleBasic` [paddingH 12, paddingV 8, border 2 (rgbHex fg1), bgColor (rgbHex fg1)]
              else
                hstack_
                  [childSpacing_ 8]
                  [ image (pack $ tabIcon ++ "-l.png")
                      `styleBasic` [width 16, height 16],
                    label (pack tabName)
                      `styleBasic` [textFont "SemiBold", textSize 16, textColor (rgbHex fg1)]
                  ]
                  `styleBasic` [paddingH 12, paddingV 8, border 2 (rgbHex fg1)]
        )
        getAllTabs

handleEventTab :: TabEnv -> TabNode -> TabModel -> TabEvent -> [EventResponse TabModel TabEvent sp ep]
handleEventTab wenv node model evt = case evt of
  TabOnClick tab -> [] 
  TabSetActive tab -> [] 
  

iconsPath :: String
iconsPath = "./resources/icons/"

getAllTabs :: [(String, String)]
getAllTabs = [("Wallpapers", iconsPath ++ "wallpaper-icon"), ("Settings", iconsPath ++ "settings-icon")]
