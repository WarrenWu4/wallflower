module UiData
  ( getTabData,
    getDirectoryData,
    getWallpaperData
  )
where

import Data.Char (toUpper)
import Data.List (zip5)
import FileParser
import Utilities (getResourcePath, moveToFront)
import DirectoryManager (getImagesInDirectories, getCurrentWallpaperPath)

-- | retrieves hard coded tab data
-- (tabId, tabIcon, tabIconId, tabLabel, tabLabelId)
getTabData :: [(String, String, String, String, String)]
getTabData = do
  let tabData = ["wallpapers", "settings"]
  let tabIds = ["tab-" ++ d | d <- tabData]
  let tabIcons = ["wallpaper-icon-d.png", "settings-icon-l.png"]
  let tabIconsId = ["tab-icon-" ++ d | d <- tabData]
  let tabLabels = [toUpper (head d) : tail d | d <- tabData] -- FIX: might be a problem
  let tabLabelsId = ["tab-label-" ++ d | d <- tabData]
  zip5 tabIds tabIcons tabIconsId tabLabels tabLabelsId

-- | retrieves directory data from settings file
-- (dirId, dirIcon, dirIconId, dirLabel, dirLabelId)
getDirectoryData :: IO [(String, String, String, String, String)]
getDirectoryData = do
  folderIcon <- getResourcePath "resources/icons/folder-icon.png"
  dirLabels <- parseSettingsFile
  let dirNums = [1 .. length dirLabels]
  let dirIds = ["directory-" ++ show num | num <- dirNums]
  let dirIcons = [folderIcon | _ <- dirNums]
  let dirIconsId = ["directory-icon-" ++ show num | num <- dirNums]
  let dirLabelsId = ["directory-label-" ++ show num | num <- dirNums]
  return $ zip5 dirIds dirIcons dirIconsId dirLabels dirLabelsId

getWallpaperData :: IO [(String, String, String)]
getWallpaperData = do
  dirs <- parseSettingsFile 
  imagePaths <- getImagesInDirectories dirs

  Just currentWallpaper <- getCurrentWallpaperPath 
  let checkWallpaper path = path == currentWallpaper
  let images = moveToFront checkWallpaper imagePaths

  let imgNums = [1 .. length images]
  let imgIds = ["image-" ++ show num | num <- imgNums]
  let imgBtnIds = ["image-btn-" ++ show num | num <- imgNums]
  return $ zip3 imgIds imgBtnIds images 






