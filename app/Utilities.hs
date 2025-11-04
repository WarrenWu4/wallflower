{-# LANGUAGE ScopedTypeVariables #-}

module Utilities where

import Control.Exception (IOException, try)
import Control.Monad (forM_, unless)
import Data.List (delete, find)
import LoggerGe
import Paths_wallflower (getDataFileName)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (normalise)
import System.Process (readProcess)

-- | returns absolute path of resource
getResourcePath :: FilePath -> IO FilePath
getResourcePath path = do
  absPath <- getDataFileName path
  return $ normalise absPath

-- | checks if a program is running using pgrep
isProgramRunning :: String -> IO Bool
isProgramRunning programName = do
  result <- try $ readProcess "pgrep" ["-x", programName] ""
  case result of
    Left (_ :: IOException) -> do
      logMsg WARNING $ "pgrep command failed, cannot reliably check status for " ++ programName
      return False
    Right _ -> return True

-- | checks if a list of resources exist
doesResourceExist :: [String] -> String -> IO ()
doesResourceExist resources resourceCheck = do
  forM_ resources $ \res -> do
    resPath <- getResourcePath res
    exists <- doesFileExist resPath
    unless exists $ logMsg ERROR (resourceCheck ++ " check failed") >> error (resPath ++ " does not exist")
  logMsg OK (resourceCheck ++ " check passed")

moveToFront :: (FilePath -> Bool) -> [FilePath] -> [FilePath]
moveToFront p xs =
  case find p xs of
    Nothing -> xs
    Just target -> target : delete target xs

getSettingsFile :: IO String
getSettingsFile = do
  getResourcePath "resources/data/directories.txt"

validateDirectory :: String -> IO Bool
validateDirectory dir = do
  exists <- doesDirectoryExist dir
  if exists
    then return True
    else do
      logMsg WARNING $ "Directory does not exist: " ++ dir ++ "\nSkipping..."
      return False

-- | gets all directories listed in the settings file
-- skips iteration if directory doesn't exist or not valid
-- @param FilePath settingsFilePath: path to settings file
-- @return [String] directoriesList: list of valid directories
readSettings :: FilePath -> IO [String]
readSettings settingsFilePath = do
  logMsg INFO $ "Reading from settings file: " ++ show settingsFilePath
  content <- readFile settingsFilePath 
  let dirs = filter (not . null) $ lines content
  validDirs <- mapM validateDirectory dirs
  let validDirList = map fst $ filter snd $ zip dirs validDirs
  return validDirList

-- | writes data to settings file 
-- @param FilePath settingsFilePath: path to settings file
-- @param [String] settingsData: data to write
-- @return ()
writeSettings:: FilePath -> [String] -> IO ()
writeSettings settingsFilePath settingsData = do
  logMsg INFO $ "Writing to settings file: " ++ show settingsFilePath
  writeFile settingsFilePath $ unlines settingsData
