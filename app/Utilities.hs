{-# LANGUAGE ScopedTypeVariables #-}

module Utilities
  ( getObjectSafe,
    getResourcePath,
    isProgramRunning,
    doesResourceExist
  )
where

import Control.Exception (IOException, try)
import Data.Text (Text)
import qualified GI.Gtk as Gtk
import LoggerGe
import Paths_wallflower (getDataFileName)
import System.Process (readProcess)
import Control.Monad (forM_, unless)
import System.Directory (doesFileExist)

-- | safely gets object from builder and casts it to the desired type
-- crashes if object is not found or cannot be casted
getObjectSafe :: (Gtk.GObject o) => Gtk.Builder -> (Gtk.ManagedPtr o -> o) -> Text -> IO o
getObjectSafe builder constructor objectId = do
  obj <- Gtk.builderGetObject builder objectId
  case obj of
    Nothing -> logMsg ERROR "Unable to get object" >> error ""
    Just o -> do
      res <- Gtk.castTo constructor o
      case res of
        Nothing -> logMsg ERROR "Cannot cast object safely" >> error ""
        Just r -> return r

-- | returns absolute path of resource
getResourcePath :: FilePath -> IO FilePath
getResourcePath path = do
  getDataFileName path


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

