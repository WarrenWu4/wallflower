module FileParser
  ( parseHyprpaperConf,
    parseSettingsFile,
    validateDirectory
  )
where

import Utilities (getResourcePath)
import System.Directory (doesDirectoryExist)
import LoggerGe

parseHyprpaperConf :: IO [(String, String)]
parseHyprpaperConf = return [("hi", "bye")] 

parseSettingsFile :: IO [String]
parseSettingsFile = do
  settingsPath <- getResourcePath "resources/data/directories.txt"
  content <- readFile settingsPath
  let dirs = filter (not . null) $ lines content
  validDirs <- mapM validateDirectory dirs
  let validDirList = map fst $ filter snd $ zip dirs validDirs
  return validDirList

validateDirectory :: String -> IO Bool
validateDirectory dir = do
  exists <- doesDirectoryExist dir
  if exists
    then return True
    else do
      logMsg WARNING $ "Directory does not exist: " ++ dir ++ "\nSkipping..."
      return False
