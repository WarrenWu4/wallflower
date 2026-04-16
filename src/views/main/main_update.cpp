#include "views/main/main.hpp"

AppModel Main_Update(AppModel m, AppMsg msg, std::string payload) {
    switch (msg) {
        case MSG_SELECT_TAB:
            // logic
            break;
        case MSG_SET_WALLPAPER:
            m.activeWallpaper = payload;
            break;
        case MSG_TOGGLE_DROPDOWN:
            m.dropdownVisible = !m.dropdownVisible;
            break;
        default: break;
    }
    return m;
}
