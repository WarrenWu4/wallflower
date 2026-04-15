#pragma once
#include <string>
#include <vector>
#include <memory>
#include "../../core/raylib.h"
#include "../../data_managers/configuration.hpp"

typedef enum {
    MSG_NONE,
    MSG_SELECT_TAB,
    MSG_SET_WALLPAPER,
    MSG_TOGGLE_DROPDOWN,
    MSG_SET_FIT_MODE,
} AppMsg;

typedef enum { TAB_GALLERY, TAB_SETTINGS, TAB_SIMPLIFIED } TabType;

typedef struct {
    TabType currentTab;
    std::string activeWallpaper;
    bool dropdownVisible;
    FitMode currentFitMode;
} AppModel;

AppModel Main_Update(AppModel m, AppMsg msg, std::string payload = "");
AppMsg   Main_View(const AppModel *m);
