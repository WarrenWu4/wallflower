{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Colors
import Control.Lens
import Monomer
import Settings
import Tabs
import Validator (checkAllDependencies)
import Wallpapers

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
  | SettingsEvt SettingsEvent
  deriving (Eq, Show)

type AppEnv = WidgetEnv AppModel AppEvent

type AppNode = WidgetNode AppModel AppEvent

makeLenses 'AppModel

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
    wallpaperWidget :: AppNode = composite_ "wallpapers" wallpaperModel buildUIWallpaper handleEventWallpaper [onInit WallpaperInit] 
    settingWidget :: AppNode = composite_ "settings" settingsModel buildUISettings handleEventSettings [onInit SettingsInit] 

handleEvent :: AppEnv -> AppNode -> AppModel -> AppEvent -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  TabEvt tabEvt -> [Message "tabWidget" tabEvt]
  WallpaperEvt wallpaperEvt -> [Message "wallpaperWidget" wallpaperEvt]
  SettingsEvt settingsEvt -> [Message "settingWidget" settingsEvt]

main :: IO ()
main = do
  checkAllDependencies
  startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Wallflower",
        appWindowIcon "./resources/icons/logo.png",
        appTheme darkTheme,
        appFontDef "Regular" "./resources/fonts/Montserrat-Regular.ttf",
        appFontDef "SemiBold" "./resources/fonts/Montserrat-SemiBold.ttf",
        appFontDef "Bold" "./resources/fonts/Montserrat-Bold.ttf",
        appInitEvent AppInit
      ]
    model =
      AppModel
        { _tabModel = defaultTabModel,
          _wallpaperModel = defaultWallpaperModel,
          _settingsModel = defaultSettingsModel
        }
