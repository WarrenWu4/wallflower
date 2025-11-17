{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Wallpapers where

import Control.Lens
import Control.Monad (zipWithM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (catMaybes)
import Data.Text (pack)
import HyprpaperManager (applyWallpaper)
import ImageDimension (getImageDimension, getImageFormat)
import LoggerGe
import Monomer
import qualified Monomer.Lens as L
import UiData (getWallpaperData)

data WallpaperModel = WallpaperModel
  { _wallpaperPaths :: [String],
    _wallpaperAspectRatios :: [Double],
    _wallpaperImages :: ByteString
  }
  deriving (Eq, Show)

data WallpaperEvent
  = WallpaperInit
  | LoadWallpapers [String]
  | LoadWallpaperDimensions [Double]
  | LoadWallpaperImages ByteString
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
      _wallpaperAspectRatios = [],
      _wallpaperImages = ""
    }

wallpaperInit :: IO WallpaperEvent
wallpaperInit =
  do
    LoadWallpapers . map (\(_, _, path) -> path)
    <$> getWallpaperData

wallpaperImageInit :: IO WallpaperEvent
wallpaperImageInit = do
  -- wallpaperData <- getWallpaperData
  -- let filePaths = map (\(_, _, path) -> path) wallpaperData
  let filePath = "/home/warrenwu/screenshots/2025-11-16_23-34.png"
  imageData <- BS.readFile filePath
  return (LoadWallpaperImages imageData)

-- imageData <- mapM BS.readFile filePaths
-- return (LoadWallpaperImages imageData)

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
      vscroll_
        [wheelRate 80]
        ( hstack_
            [childSpacing_ 12]
            [ vstack_ [childSpacing_ 12] col1 `styleBasic` [],
              vstack_ [childSpacing_ 12] col2 `styleBasic` [],
              vstack_ [childSpacing_ 12] col3 `styleBasic` []
            ]
        )

    imageButton :: (String, Double) -> WallpaperNode
    imageButton (p, ar) =
      box_
        [onClick (SetWallpaper p)]
        ( image_ (pack p) [fitWidth] `styleBasic` [width columnWidth, height $ columnWidth * ar]
        )

    -- static image issue caused by wrong encoding format in bytestring
    -- testImage :: WallpaperNode = imageMem_ "testing" (model ^. wallpaperImages) Size{_sW=55, _sH=47} []

    indexedList = zip3 [0 ..] (model ^. wallpaperPaths) (model ^. wallpaperAspectRatios)
    col1 :: [WallpaperNode] = map imageButton $ [(p, ar) | (i, p, ar) <- indexedList, i `mod` 3 == 0]
    col2 :: [WallpaperNode] = map imageButton $ [(p, ar) | (i, p, ar) <- indexedList, i `mod` 3 == 1]
    col3 :: [WallpaperNode] = map imageButton $ [(p, ar) | (i, p, ar) <- indexedList, i `mod` 3 == 2]

handleEventWallpaper :: WallpaperEnv -> WallpaperNode -> WallpaperModel -> WallpaperEvent -> [EventResponse WallpaperModel WallpaperEvent sp ep]
handleEventWallpaper wenv node model evt = case evt of
  WallpaperInit -> [Task wallpaperInit, Task wallpaperImageInit, Task wallpaperDimInit]
  LoadWallpapers paths -> [Model $ model & wallpaperPaths .~ paths]
  LoadWallpaperDimensions dims -> [Model $ model & wallpaperAspectRatios .~ dims]
  LoadWallpaperImages images -> [Model $ model & wallpaperImages .~ images]
  SetWallpaper path -> [Task $ setWallpaperHandler path]
  WallpaperNone -> []
