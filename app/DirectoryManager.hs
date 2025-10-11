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
import LoggerGe
import Utilities

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

-- | get directories from setting file
-- if the settings file doesn't exist create a default one
getDirectoriesFromSetting :: IO [FilePath]
getDirectoriesFromSetting = do
  logMsg INFO "Fetching directories from settings"
  directoriesPath <- getResourcePath "resources/data/directories.txt"
  exists <- doesFileExist directoriesPath
  if not exists
    then do
      logMsg WARNING "Settings file not found... Please report this error to the GitHub...\n" 
      -- TODO: set fallback by creating dfeualt settings file
      -- logMsg DEBUG "Creating default settings file"
      logMsg DEBUG "Creating default background directory"
      homeDirectory <- getHomeDirectory
      let defaultDir = homeDirectory </> "backgrounds"
      createDirectoryIfMissing True defaultDir
      return [defaultDir]
    else do
      contents <- readFile directoriesPath
      let dirs = filter (not . null) $ lines contents
      return dirs

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


