{-# LANGUAGE OverloadedStrings #-}

module Tabs
  ( getAllTabs,
  )
where

-- INFO: derives if tab is active

iconsPath :: String
iconsPath = "./resources/icons/"

getAllTabs :: [(String, String)]
getAllTabs = [("Wallpaper", iconsPath ++ "wallpaper-icon"), ("Settings", iconsPath ++ "settings-icon")]
