module UiData
  ( getActionIds,
    getActionLabels,
    getActionButtonIds,
    getActionIconIds,
    getActionIcons,
  )
where

getActionIds :: [String]
getActionIds = ["wallpapers", "settings"]

getActionLabels :: [String]
getActionLabels = ["Wallpapers", "Settings"]

getActionButtonIds :: [String]
getActionButtonIds = ["btn-" ++ a | a <- getActionIds]

getActionIconIds :: [String]
getActionIconIds = ["btn-icon-" ++ a | a <- getActionIds]

getActionIcons :: [String]
getActionIcons = ["resources/icons/wallpaper-icon-d.png", "resources/icons/settings-icon-l.png"]
