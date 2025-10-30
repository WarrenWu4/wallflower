{-# LANGUAGE ScopedTypeVariables #-}

module Utilities
  ( getResourcePath,
    isProgramRunning,
    doesResourceExist,
    moveToFront,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (forM_, unless)
import Data.List (delete, find)
import LoggerGe
import Paths_wallflower (getDataFileName)
import System.Directory (doesFileExist)
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
