{-# LANGUAGE OverloadedStrings #-}

module TemplateBuilder 
  ( generateTabMarkup,
    generateWallpaperMarkup,
    generateDirectoryListMarkup,
    buildTemplate
  )
where

import qualified Data.Text as T
import Utilities
import UiData 

generateTabMarkup :: IO String
generateTabMarkup = do
  let tabData = getTabData
  templatePath <- getResourcePath "resources/templates/tab.template"
  template <- readFile templatePath
  results <- mapM (\(tabId, tabIcon, tabIconId, tabLabel, tabLabelId) -> do
    absIconPath <- getResourcePath $ "resources/icons/" ++ tabIcon
    let mappings = [("{tab-id}", tabId), ("{tab-icon-id}", tabIconId), ("{tab-label}", tabLabel), ("{tab-label-id}", tabLabelId), ("{icon-path}", absIconPath)]
    return $ foldl (\acc (placeholder, value) -> T.replace (T.pack placeholder) (T.pack value) acc) (T.pack template) mappings) tabData
  return (T.unpack $ T.concat results)

generateWallpaperMarkup :: IO String
generateWallpaperMarkup = do
  wallpaperData <- getWallpaperData
  templatePath <- getResourcePath "resources/templates/image.template"
  template <- readFile templatePath
  results <- mapM (\(imgId, imgBtnId, imgPath) -> do
    let mappings = [("{image-btn-id}", imgBtnId), ("{image-id}", imgId), ("{image-path}", imgPath)]
    return $ foldl (\acc (placeholder, value) -> T.replace (T.pack placeholder) (T.pack value) acc) (T.pack template) mappings) wallpaperData 
  return (T.unpack $ T.concat results)

generateDirectoryListMarkup ::  IO String
generateDirectoryListMarkup  = do
  directoryData <- getDirectoryData
  templatePath <- getResourcePath "resources/templates/directory.template"
  template <- readFile templatePath
  absIconPath <- getResourcePath "resources/icons/folder-icon.png" 
  results <- mapM (\(dirId, _, dirIconId, dirLabel, dirLabelId) -> do
    let mappings = [("{directory-id}", dirId), ("{directory-icon-id}", dirIconId), ("{directory-label-id}", dirLabelId), ("{dir-label}", dirLabel), ("{icon-path}", absIconPath)]
    return $ foldl (\acc (placeholder, value) -> T.replace (T.pack placeholder) (T.pack value) acc) (T.pack template) mappings) directoryData 
  return (T.unpack $ T.concat results)

-- | wraps content in proper header/footer and saves it in a file
buildTemplate :: String -> FilePath -> IO ()
buildTemplate content filePath = do
  let header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<interface>\n\t<requires lib=\"gtk\" version=\"4.0\"/>\n"
  let footer = "\n</interface>"
  writeFile filePath $ header ++ content ++ footer

-- -- | attaches action to image button to apply wallpaper
-- -- @buidler (Gtk.Builder): gtk builder object
-- -- @numId (Int): unique identifier for button & image
-- -- @imgPath (FilePath): path to image file
-- -- @returns (IO ()): nothing
-- applyBackgroundAction :: Gtk.Builder -> Int -> FilePath -> IO ()
-- applyBackgroundAction builder numId imgPath = do
--   let btnId = "btn-background-image-" ++ show numId
--   btn <- getObjectSafe builder Gtk.Button (pack btnId)
--   _ <- Gtk.on btn #clicked $ do
--     logMsg DEBUG $ "Applying wallpaper: " ++ imgPath
--     applyWallpaper imgPath
--   return ()
