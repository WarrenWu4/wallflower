#include "views/root/root.hpp"
#include "core/clay.h"
#include "core/colors.h"

RootModel Root_Init() {
    return {
        .searchBarModel = SearchBar_Init(),
        .galleryModel = Gallery_Init(),
    };
}

RootModel Root_Update(RootModel m, RootMsg msg, std::string payload) {
    switch (msg) {
    case MSG_NONE:
        break;
    default:
        break;
    }
    return m;
}

RootModel Root_View(RootModel m) {

    CLAY(CLAY_ID("RootContainer"),
         {
             .layout = {.sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
                        .padding = {CLAY_PADDING_ALL(16)},
                        .childGap = 16,
                        .layoutDirection = CLAY_TOP_TO_BOTTOM},
             .backgroundColor = COLOR_BACKGROUND_1,
         }) {
        m.searchBarModel = SearchBar_View(m.searchBarModel);
        m.galleryModel = Gallery_View(m.galleryModel);
    }

    return m;
}
