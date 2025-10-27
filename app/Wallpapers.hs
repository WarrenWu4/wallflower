{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Wallpapers where

import Control.Lens
import Monomer
import LoggerGe

data WallpaperModel = WallpaperModel
  { _wallpaperPaths :: [String]
  }
  deriving (Eq, Show)

defaultWallpaperModel :: WallpaperModel
defaultWallpaperModel =
  WallpaperModel
    { _wallpaperPaths = []
    }

data WallpaperEvent
  = LoadWallpapers [String]
  | SetWallpaper String
  | ResizeWallpaper (Int, Int)
  deriving (Eq, Show)

makeLenses 'WallpaperModel

buildUIWallpaper :: WidgetEnv WallpaperModel WallpaperEvent -> WallpaperModel -> WidgetNode WallpaperModel WallpaperEvent
buildUIWallpaper wenv model = widgetTree
  where
    widgetTree = vscroll (
      hstack_ [childSpacing_ 12] $
      [label "Wallpaper Module"] ++ testItems
      )
    testItems = map (\paths -> label "Testing") (model ^. wallpaperPaths)

handleEventWallpaper :: WidgetEnv WallpaperModel WallpaperEvent -> WidgetNode WallpaperModel WallpaperEvent -> WallpaperModel -> WallpaperEvent -> [EventResponse WallpaperModel WallpaperEvent sp ep]
handleEventWallpaper wenv node model evt = case evt of
  LoadWallpapers paths -> [Model $ model & wallpaperPaths .~ paths]
  SetWallpaper path -> []
  ResizeWallpaper (w, h) -> []
