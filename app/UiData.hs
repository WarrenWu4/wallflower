module UiData
  ( getActionIds,
    getActionButtonId,
    getActionIconId,
    getActionLabelId,
    getActionLabels,
    getActionButtonIds,
    getActionIconIds,
    getActionIcons,
    getFolderIcon
  )
where

getActionIds :: [String]
getActionIds = ["wallpapers", "settings"]

getActionButtonId :: String -> String
getActionButtonId action = "btn-" ++ action

getActionIconId :: String -> String
getActionIconId action = "btn-icon-" ++ action

getActionLabelId :: String -> String
getActionLabelId action = "btn-label-" ++ action

getActionLabels :: [String]
getActionLabels = ["Wallpapers", "Settings"]

getActionButtonIds :: [String]
getActionButtonIds = [getActionButtonId a | a <- getActionIds]

getActionIconIds :: [String]
getActionIconIds = [getActionIconId a | a <- getActionIds]

getActionIcons :: [String]
getActionIcons = ["resources/icons/wallpaper-icon-d.png", "resources/icons/settings-icon-l.png"]

getFolderIcon :: String
getFolderIcon = "resources/icons/folder-icon.png"
