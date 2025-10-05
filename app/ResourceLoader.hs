{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module ResourceLoader
  ( loadResource,
    getResourcePath,
    loadUI,
    loadCSS,
  )
where

import Control.Monad (forM_)
import Data.Int
import Data.Text.Internal
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import MarkupInjector
import Paths_wallflower (getDataFileName)

loadResource :: FilePath -> IO String
loadResource path = do
  absPath <- getDataFileName path
  readFile absPath

getResourcePath :: FilePath -> IO FilePath
getResourcePath path = do
  getDataFileName path

getImageById :: Gtk.Builder -> Text -> IO Gtk.Picture
getImageById builder objectId = do
  Just obj <- Gtk.builderGetObject builder objectId
  Gtk.unsafeCastTo Gtk.Picture obj

placeImages :: Gtk.Grid -> [Gtk.Picture] -> IO ()
placeImages container imgs = do
  let indexedImgs = zip [0..(length imgs - 1)] imgs
  forM_ indexedImgs $ \(i, img) -> do
    let num = fromIntegral i :: Int32
    let col = num `mod` 3 :: Int32
    let row = num `div` 3 :: Int32
    Gtk.gridAttach container img col row 1 1

loadUI :: Gtk.Application -> [String] -> IO ()
loadUI app imageMarkups = do
  createTempFile $ concat imageMarkups
  imageFiles <- getResourcePath "resources/images.ui"

  uiFile <- getResourcePath "resources/window.ui"
  builder <- Gtk.builderNew
  _ <- Gtk.builderAddFromFile builder uiFile
  _ <- Gtk.builderAddFromFile builder imageFiles

  Just winObj <- Gtk.builderGetObject builder "main_window"
  window <- Gtk.unsafeCastTo Gtk.ApplicationWindow winObj

  let imageIds = ["background-image-" ++ show n | n <- [1 .. length imageMarkups]]
  imgs <- mapM (getImageById builder . pack) imageIds

  Just imgContainerObj <- Gtk.builderGetObject builder "backgrounds"
  imgContainer <- Gtk.unsafeCastTo Gtk.Grid imgContainerObj

  placeImages imgContainer imgs

  #setApplication window (Just app)

  #show window

loadCSS :: IO ()
loadCSS = do
  cssPath <- getResourcePath "resources/style.css"
  provider <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromPath provider cssPath
  display <- Gdk.displayGetDefault
  case display of
    Nothing -> putStrLn "Failed to get default display"
    Just d -> do
      Gtk.styleContextAddProviderForDisplay
        d
        provider
        (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)
