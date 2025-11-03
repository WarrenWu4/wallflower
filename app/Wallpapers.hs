{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Wallpapers where

import Control.Lens
import Data.Text (pack)
import Monomer
import qualified Monomer.Lens as L

data WallpaperModel = WallpaperModel
  { _wallpaperPaths :: [String],
    _wallpaperAspectRatios :: [Double]
  }
  deriving (Eq, Show)

data WallpaperEvent
  = LoadWallpapers [String]
  | LoadWallpaperDimensions [Double]
  | SetWallpaper String
  | ResizeWallpaper (Int, Int)
  deriving (Eq, Show)

type WallpaperEnv = WidgetEnv WallpaperModel WallpaperEvent

type WallpaperNode = WidgetNode WallpaperModel WallpaperEvent

makeLenses 'WallpaperModel

defaultWallpaperModel :: WallpaperModel
defaultWallpaperModel =
  WallpaperModel
    { _wallpaperPaths = [],
      _wallpaperAspectRatios = []
    }

-- TODO: figure some better way to do this dynamic sizing crap
getWallpaperColumnSize :: Double -> Double
getWallpaperColumnSize windowWidth = do
  -- windowPadding references padding value in Main.hs
  let windowPadding = 24
  let columnGap = 12
  let numColumns = 3
  (windowWidth - windowPadding * 2 - columnGap * (numColumns - 1)) / numColumns

buildUIWallpaper :: WallpaperEnv -> WallpaperModel -> WallpaperNode
buildUIWallpaper wenv model = widgetTree
  where
    windowWidth = wenv ^. (L.windowSize . L.w)
    columnWidth = getWallpaperColumnSize windowWidth

    widgetTree =
      vscroll
        ( hstack_
            [childSpacing_ 12]
            [ vstack_ [childSpacing_ 12] col1 `styleBasic` [],
              vstack_ [childSpacing_ 12] col2 `styleBasic` [],
              vstack_ [childSpacing_ 12] col3 `styleBasic` []
            ]
        )

    indexedList = zip3 [0..] (model ^. wallpaperPaths) (model ^. wallpaperAspectRatios)
    col1 :: [WallpaperNode] = map (\(path, aspectRatio) -> image_ (pack path) [fitWidth] `styleBasic` [width columnWidth, height (aspectRatio * columnWidth)]) $ [(p, ar) | (i, p, ar) <- indexedList, i `mod` 3 == 0] 
    col2 :: [WallpaperNode] = map (\(path, aspectRatio) -> image_ (pack path) [fitWidth] `styleBasic` [width columnWidth, height (aspectRatio * columnWidth)]) $ [(p, ar) | (i, p, ar) <- indexedList, i `mod` 3 == 1] 
    col3 :: [WallpaperNode] = map (\(path, aspectRatio) -> image_ (pack path) [fitWidth] `styleBasic` [width columnWidth, height (aspectRatio * columnWidth)]) $ [(p, ar) | (i, p, ar) <- indexedList, i `mod` 3 == 2] 

handleEventWallpaper :: WallpaperEnv -> WallpaperNode -> WallpaperModel -> WallpaperEvent -> [EventResponse WallpaperModel WallpaperEvent sp ep]
handleEventWallpaper wenv node model evt = case evt of
  LoadWallpapers paths -> [Model $ model & wallpaperPaths .~ paths]
  LoadWallpaperDimensions dims -> [Model $ model & wallpaperAspectRatios .~ dims]
  SetWallpaper path -> []
  ResizeWallpaper (w, h) -> []
