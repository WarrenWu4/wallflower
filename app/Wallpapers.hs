{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Wallpapers where

import Control.Lens
import Data.Text (pack)
import Monomer
import qualified Monomer.Lens as L
import LoggerGe
import HyprpaperManager (applyWallpaper)
import UiData (getWallpaperData)
import ImageDimension (getImageFormat, getImageDimension)
import Control.Monad (zipWithM)
import Data.Maybe (catMaybes)

data WallpaperModel = WallpaperModel
  { _wallpaperPaths :: [String],
    _wallpaperAspectRatios :: [Double]
  }
  deriving (Eq, Show)

data WallpaperEvent
  = WallpaperInit
  | LoadWallpapers [String]
  | LoadWallpaperDimensions [Double]
  | SetWallpaper String
  | WallpaperNone
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

wallpaperInit :: IO WallpaperEvent
wallpaperInit = do
  wallpaperData <- getWallpaperData
  return (LoadWallpapers $ map (\(_, _, path) -> path) wallpaperData)

wallpaperDimInit :: IO WallpaperEvent
wallpaperDimInit = do
  wallpaperData <- getWallpaperData
  let filePaths = map (\(_, _, path) -> path) wallpaperData
  let fileExtensions = map getImageFormat filePaths
  results <- zipWithM getImageDimension filePaths fileExtensions
  -- INFO: reverse h/w because future sizing is dependent on width thus making dynamic resizing easier to calculate
  let validResults :: [(Int, Int)] = catMaybes results
  let aspectRatios = [(\num den -> fromIntegral num / fromIntegral den) h w | (w, h) <- validResults]
  return $ LoadWallpaperDimensions aspectRatios


-- TODO: figure some better way to do this dynamic sizing crap
getWallpaperColumnSize :: Double -> Double
getWallpaperColumnSize windowWidth = do
  -- windowPadding references padding value in Main.hs
  let windowPadding = 24
  let columnGap = 12
  let numColumns = 3
  (windowWidth - windowPadding * 2 - columnGap * (numColumns - 1)) / numColumns

setWallpaperHandler :: String -> IO WallpaperEvent 
setWallpaperHandler path = do
  logMsg DEBUG $ "Setting wallpaper to: " ++ path
  applyWallpaper path
  return WallpaperNone 

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

    imageButton :: String -> Double -> WallpaperNode
    imageButton p ar = box_ [onClick (SetWallpaper p)] (image_ (pack p) [fitWidth] `styleBasic` [width columnWidth, height (ar * columnWidth)])

    indexedList = zip3 [0..] (model ^. wallpaperPaths) (model ^. wallpaperAspectRatios)
    col1 :: [WallpaperNode] = map (uncurry imageButton) $ [(p, ar) | (i, p, ar) <- indexedList, i `mod` 3 == 0] 
    col2 :: [WallpaperNode] = map (uncurry imageButton) $ [(p, ar) | (i, p, ar) <- indexedList, i `mod` 3 == 1] 
    col3 :: [WallpaperNode] = map (uncurry imageButton) $ [(p, ar) | (i, p, ar) <- indexedList, i `mod` 3 == 2] 

handleEventWallpaper :: WallpaperEnv -> WallpaperNode -> WallpaperModel -> WallpaperEvent -> [EventResponse WallpaperModel WallpaperEvent sp ep]
handleEventWallpaper wenv node model evt = case evt of
  WallpaperInit -> [Task wallpaperInit, Task wallpaperDimInit]
  LoadWallpapers paths -> [Model $ model & wallpaperPaths .~ paths]
  LoadWallpaperDimensions dims -> [Model $ model & wallpaperAspectRatios .~ dims]
  SetWallpaper path -> [Task $ setWallpaperHandler path]
  WallpaperNone -> []
