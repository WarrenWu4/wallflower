module DirectoryManager (
  isImage,
  getImagesInDirectory,
  getImagesInDirectories,
  getDirectoriesFromSetting,
  saveDirectoriesToSetting,
  getHyprpaperConfigPath,
  getCurrentWallpaperPath,
  isCurrentWallpaper
) where

import System.Directory
import System.FilePath
import Control.Monad
import Data.List.Split (splitOn)

-- | checks if file path is a supported image
-- supported image types: jpg, jpeg, png
isImage :: FilePath -> Bool
isImage f = do
  let ext = last $ splitOn "." f  
  case map toLower (ext) of
    "jpg"  -> True
    "jpeg" -> True
    "png"  -> True
    _       -> False
  where
    toLower c
      | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise            = c

-- | list all images in a directory
-- absolute paths only
getImagesInDirectory :: FilePath -> IO [FilePath]
getImagesInDirectory d = do
  contents <- listDirectory d
  let fullPaths = filter isImage $ map (d </>) contents
  filterM doesFileExist fullPaths 

-- |list all images in a list of directories
getImagesInDirectories :: [FilePath] -> IO [FilePath]
getImagesInDirectories dirs = do
  imagesList <- mapM getImagesInDirectory dirs
  return $ concat imagesList

-- FIX: hardcode for now
-- | get directories from setting file
getDirectoriesFromSetting _ = ["/home/warrenwu/backgrounds"]
-- getDirectoriesFromSetting :: FilePath -> IO [FilePath]

-- TODO: implement once settings file is figured out
-- | save directories to setting file
saveDirectoriesToSetting :: FilePath -> [FilePath] -> IO ()
saveDirectoriesToSetting settingPath dirs = return ()

getHyprpaperConfigPath :: IO FilePath
getHyprpaperConfigPath = do
  homeDir <- getHomeDirectory 
  return (homeDir </> ".config" </> "hypr" </> "hyprpaper.conf")

getCurrentWallpaperPath :: IO (Maybe FilePath)
getCurrentWallpaperPath = do
  configPath <- getHyprpaperConfigPath
  exists <- doesFileExist configPath
  if not exists
    then return Nothing
  else do 
    contents <- readFile configPath
    let strPath = last $ splitOn " " contents
    let parsedPath = filter (\c -> c /= '"' && c /= '\n') strPath
    return (Just parsedPath)

isCurrentWallpaper :: FilePath -> IO Bool
isCurrentWallpaper path = do
  currentWallpaper <- getCurrentWallpaperPath
  case currentWallpaper of
    Just p  -> return (p == path)
    Nothing -> return False 


