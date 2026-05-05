#include "views/search_bar/search_bar.hpp"
#include "core/clay.h"
#include "core/colors.h"

SearchBarModel SearchBar_Init() {
    return {
        .searchResults = {},
        .searchOptions = {},
        .activeOption = 0,
    };
}

SearchBarModel SearchBar_Update(SearchBarModel m, SearchBarMsg msg,
                                std::string payload) {
    switch (msg) {
    default:
        break;
    }
    return m;
}

SearchBarModel SearchBar_View(SearchBarModel m) {

    CLAY(CLAY_ID("SearchBarContainer"),
         {
             .layout = {.sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
                        .padding = {CLAY_PADDING_ALL(16)},
                        .childGap = 16,
                        .layoutDirection = CLAY_TOP_TO_BOTTOM},
             .backgroundColor = COLOR_FOREGROUND_3,
         }) {}

    return m;
}
