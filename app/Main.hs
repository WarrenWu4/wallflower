{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import LoggerGe 
import Control.Lens
import Monomer
import Validator (checkAllDependencies)
import Tabs 
import Data.Text (pack)
import Colors

data AppModel = AppModel
  { _title :: String,
    _activeTab :: String
  }
  deriving (Eq, Show)

data AppEvent
  = AppInit
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI ::
  WidgetEnv AppModel AppEvent -> AppModel -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    widgetTree =
      vstack_
        [childSpacing_ 20]
        [ 
          hstack_ [childSpacing_ 16] tabUIs
        ]
        `styleBasic` [padding 24, bgColor (rgbHex bg1)]

    tabUIs = map (\(tabName, tabIcon) -> 
      -- let tabIsActive = tabName == (model ^. activeTab) 
      hstack_ [childSpacing_ 8] [ 
        label (pack tabName) 
        `styleBasic` [textFont "SemiBold", textSize 16, textColor (rgbHex fg1)]
      ] 
      `styleBasic` [paddingH 12, paddingV 8, border 2 (rgbHex fg1)]
      ) getAllTabs

handleEvent :: WidgetEnv AppModel AppEvent -> WidgetNode AppModel AppEvent -> AppModel -> AppEvent -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []

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
        { _title = "Wallflower",
          _activeTab = "Wallpaper" 
        }
