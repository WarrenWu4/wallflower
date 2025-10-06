module Utilities (
    getObjectSafe
) where

import Data.Text (Text)
import LoggerGe
import qualified GI.Gtk as Gtk

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
