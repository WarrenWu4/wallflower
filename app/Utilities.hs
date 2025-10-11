module Utilities (
    getObjectSafe,
    getResourcePath
) where

import Data.Text (Text)
import LoggerGe
import qualified GI.Gtk as Gtk
import Paths_wallflower (getDataFileName)

-- | safely gets object from builder and casts it to the desired type
-- crashes if object is not found or cannot be casted
getObjectSafe :: (Gtk.GObject o) => Gtk.Builder -> (Gtk.ManagedPtr o -> o) -> Text -> IO o
getObjectSafe builder constructor objectId = do
  obj <- Gtk.builderGetObject builder objectId
  case obj of
    Nothing -> logMsg ERROR "Unable to get object" >> error "" 
    Just o  -> do
        res <- Gtk.castTo constructor o
        case res of
          Nothing -> logMsg ERROR "Cannot cast object safely" >> error ""
          Just r  -> return r

-- | returns absolute path of resource
getResourcePath :: FilePath -> IO FilePath
getResourcePath path = do
  getDataFileName path

