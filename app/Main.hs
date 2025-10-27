{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Colors
import Control.Lens
import LoggerGe
import Monomer
import Tabs
import UiData (getWallpaperData)
import Validator (checkAllDependencies)
import Wallpapers
import Settings

data AppModel = AppModel
  { _tabModel :: TabModel,
    _wallpaperModel :: WallpaperModel,
    _settingsModel :: SettingsModel
  }
  deriving (Eq, Show)

data AppEvent
  = AppInit
  | TabEvt TabEvent
  | WallpaperEvt WallpaperEvent
  deriving (Eq, Show)

type AppEnv = WidgetEnv AppModel AppEvent

type AppNode = WidgetNode AppModel AppEvent

makeLenses 'AppModel

wallpaperInit :: IO AppEvent
wallpaperInit = do
  logMsg DEBUG "Loading wallpapers"
  wallpaperData <- getWallpaperData
  let state = LoadWallpapers $ map (\(_, _, path) -> path) wallpaperData
  return (WallpaperEvt state)

buildUI :: AppEnv -> AppModel -> AppNode
buildUI wenv model = widgetTree
  where
    widgetTree =
      vstack_
        [childSpacing_ 20]
        [ composite "tab" tabModel buildUITab handleEventTab `nodeKey` "tabWidget",
          if (model ^. (tabModel . tabActive)) == "Wallpapers"
            then wallpaperWidget
            else settingWidget 
        ]
        `styleBasic` [padding 24, bgColor (rgbHex bg1)]
    wallpaperWidget :: AppNode = composite "wallpapers" wallpaperModel buildUIWallpaper handleEventWallpaper `nodeKey` "wallpaperWidget"
    settingWidget :: AppNode = composite "settings" settingsModel buildUISettings handleEventSettings `nodeKey` "settingWidget" 
handleEvent :: AppEnv -> AppNode -> AppModel -> AppEvent -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> [Task wallpaperInit]
  TabEvt tabEvt -> [Message "tabWidget" tabEvt]
  WallpaperEvt wallpaperEvt -> [Message "wallpaperWidget" wallpaperEvt]

main :: IO ()
main = do
  -- todo: uncomment later
  -- checkAllDependencies
  startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Wallflower",
        appWindowIcon "./resources/icons/logo.png",
        appTheme darkTheme,
        appFontDef "Regular" "./assets/Montserrat-Regular.ttf",
        appFontDef "SemiBold" "./assets/Montserrat-SemiBold.ttf",
        appFontDef "Bold" "./assets/Montserrat-Bold.ttf",
        appInitEvent AppInit
      ]
    model =
      AppModel
        { _tabModel = defaultTabModel,
          _wallpaperModel = defaultWallpaperModel,
          _settingsModel = defaultSettingsModel
        }
