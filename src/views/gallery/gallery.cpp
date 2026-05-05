#include "views/gallery/gallery.hpp"
#include "core/clay.h"
#include "core/colors.h"

GalleryModel Gallery_Init() {
    return {.activeWallpapers = {}, .availableWallpapers = {}};
}

GalleryModel Gallery_Update(GalleryModel m, GalleryMsg msg,
                            std::string payload) {
    switch (msg) {
    case MSG_SET_WALLPAPER:
        m.activeWallpapers.push_back(payload);
        break;
    default:
        break;
    }
    return m;
}

GalleryMsg Gallery_View(const GalleryModel *m) {
    GalleryMsg result = MSG_NONE;

    CLAY(CLAY_ID("GalleryContainer"),
         {
             .layout = {.sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
                        .padding = {CLAY_PADDING_ALL(16)},
                        .childGap = 16,
                        .layoutDirection = CLAY_TOP_TO_BOTTOM},
             .backgroundColor = COLOR_FOREGROUND_3,
         }) {}

    return result;
}
