{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Wallpapers where

import Control.Lens
import Data.Text (pack)
import Monomer
import qualified Monomer.Lens as L

data WallpaperModel = WallpaperModel
  { _wallpaperPaths :: [String]
  }
  deriving (Eq, Show)

data WallpaperEvent
  = LoadWallpapers [String]
  | SetWallpaper String
  | ResizeWallpaper (Int, Int)
  deriving (Eq, Show)

type WallpaperEnv = WidgetEnv WallpaperModel WallpaperEvent

type WallpaperNode = WidgetNode WallpaperModel WallpaperEvent

makeLenses 'WallpaperModel

defaultWallpaperModel :: WallpaperModel
defaultWallpaperModel =
  WallpaperModel
    { _wallpaperPaths = []
    }

-- TODO: figure some better way to do this dynamic sizing crap
getWallpaperColumnSize :: Double -> Double
getWallpaperColumnSize windowWidth = do
  -- windowPadding references padding value in Main.hs
  let windowPadding = 24
  let columnGap = 12
  let numColumns = 3
  (windowWidth - windowPadding * 2 - columnGap * (numColumns - 1)) / numColumns

-- getWallpaperDimensions :: String -> IO (Maybe (Int, Int))
-- getWallpaperDimensions path = do
--   -- readImageRGB loads the image data in a common format.
--   imgE <- readImageRGB path
--   case imgE of
--     -- If successful (Right), extract dimensions.
--     -- Note: 'rows' is height, and 'cols' is width.
--     Right img -> do
--       let w = cols (img :: Image VU PixelRGB Double)
--       let h = rows (img :: Image VU PixelRGB Double)
--       return $ Just (w, h)
--     -- If reading fails (Left), return Nothing and print the error.
--     Left err  -> do
--       putStrLn $ "Error reading image: " ++ err
--       return Nothing

buildUIWallpaper :: WallpaperEnv -> WallpaperModel -> WallpaperNode
buildUIWallpaper wenv model = widgetTree
  where
    windowWidth = wenv ^. (L.windowSize . L.w)
    columnWidth = getWallpaperColumnSize windowWidth 
  

    widgetTree =
      vscroll
        ( hstack_
            [childSpacing_ 12]
            [ vstack testItems `styleBasic` [width columnWidth, border 2 (rgbHex "#ff0000")],
              vstack [label $ pack $ show windowWidth] `styleBasic` [width columnWidth, border 2 (rgbHex "#ff0000")],
              vstack [label $ pack $ show (getWallpaperColumnSize windowWidth)] `styleBasic` [width columnWidth, border 2 (rgbHex "#ff0000")]
            ]
        )

    testItems = map (\path -> label $ pack path) (model ^. wallpaperPaths)

handleEventWallpaper :: WallpaperEnv -> WallpaperNode -> WallpaperModel -> WallpaperEvent -> [EventResponse WallpaperModel WallpaperEvent sp ep]
handleEventWallpaper wenv node model evt = case evt of
  LoadWallpapers paths -> [Model $ model & wallpaperPaths .~ paths]
  SetWallpaper path -> []
  ResizeWallpaper (w, h) -> []
