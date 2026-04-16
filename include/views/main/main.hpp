#pragma once
#include <string>
#include "data_managers/configuration.hpp"

// event/message definitions
typedef enum {
    MSG_NONE,
    MSG_SELECT_TAB,
    MSG_SET_WALLPAPER,
    MSG_TOGGLE_DROPDOWN,
    MSG_SET_FIT_MODE,
} AppMsg;

typedef enum { TAB_GALLERY, TAB_SETTINGS, TAB_SIMPLIFIED } TabType;

// define the app data
typedef struct {
    TabType currentTab;
    std::string activeWallpaper;
    bool dropdownVisible;
    FitMode currentFitMode;
} AppModel;

// view functions (update to handle events and view to render UI)
AppModel Main_Update(AppModel m, AppMsg msg, std::string payload = "");
AppMsg   Main_View(const AppModel *m);
