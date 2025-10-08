{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module MarkupInjector
  ( createTempFile,
    createImageMarkup,
    buildImageTemplate,
    applyBackgroundAction
  )
where

import Data.Text (pack)
import qualified Data.Text as T
import DirectoryManager (isCurrentWallpaper)
import qualified GI.Gtk as Gtk
import HyprpaperManager
import Utilities
import LoggerGe

-- | writes temporary file with image ui markup
createTempFile :: String -> IO ()
createTempFile content = do
  let header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<interface>\n\t<requires lib=\"gtk\" version=\"4.0\"/>\n"
  let footer = "\n</interface>"
  writeFile "resources/images.ui" $ header ++ content ++ footer

-- | for an image path, creates a GtkPicture widget
createImageMarkup :: FilePath -> Int -> IO String
createImageMarkup path imageNum = do
  isActive <- isCurrentWallpaper path
  return ("\t<object class=\"GtkPicture\" id=\"background-image-" ++ show imageNum ++ "\">" ++ "\n\t\t<property name=\"file\">" ++ path ++ "</property>" ++ "\n\t\t<property name=\"css-classes\">" ++ (if isActive then "wallpaper-active" else "wallpaper") ++ "</property>" ++ "\n\t</object>\n")

-- | based on image template fills variables
-- @imgPath (FilePath): path to image file
-- @numId (Int): unique identifier for button & image
-- @returns (IO String): filled template in string format
buildImageTemplate :: FilePath -> Int -> IO String
buildImageTemplate imgPath numId = do
  let btnId = "btn-background-image-" ++ show numId
  let imgId = "background-image-" ++ show numId
  template <- readFile "resources/image.template"
  let withBtnId = T.replace (T.pack "{btn_id}") (T.pack btnId) (T.pack template)
  let withImgId = T.replace (T.pack "{img_id}") (T.pack imgId) withBtnId
  let withImgPath = T.replace (T.pack "{img_path}") (T.pack imgPath) withImgId
  return (T.unpack withImgPath)

-- | attaches action to image button to apply wallpaper
-- @buidler (Gtk.Builder): gtk builder object
-- @numId (Int): unique identifier for button & image
-- @imgPath (FilePath): path to image file
-- @returns (IO ()): nothing
applyBackgroundAction :: Gtk.Builder -> Int -> FilePath -> IO ()
applyBackgroundAction builder numId imgPath = do
  let btnId = "btn-background-image-" ++ show numId
  btn <- getObjectSafe builder Gtk.Button (pack btnId)
  _ <- Gtk.on btn #clicked $ do 
    logMsg DEBUG $ "Applying wallpaper: " ++ imgPath
    applyWallpaper imgPath
  return ()
