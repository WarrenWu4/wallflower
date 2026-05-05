#include "views/search_bar/search_bar.hpp"
#include "core/clay.h"
#include "core/colors.h"

SearchBarModel SearchBar_Init() {
    return {
        .query = "",
        .placeholder = "Search wallpapers...",
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
         {.layout =
              {
                  .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_FIXED(40)},
                  .padding = {CLAY_PADDING_ALL(8)},
                  .childGap = 8,
                  .childAlignment = {.x = CLAY_ALIGN_X_LEFT,
                                     .y = CLAY_ALIGN_Y_CENTER},
              },
          .backgroundColor = COLOR_BACKGROUND_2}) {

        CLAY(CLAY_ID("SearchBarIcon"),
             {
                 .layout = {.sizing = {CLAY_SIZING_FIXED(16),
                                       CLAY_SIZING_FIXED(16)}},
                 .backgroundColor = COLOR_FOREGROUND_3,
             }) {}

        CLAY(CLAY_ID("SearchBarInput"),
             {
                 .layout =
                     {
                         .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
                         .childAlignment = {.x = CLAY_ALIGN_X_LEFT,
                                            .y = CLAY_ALIGN_Y_CENTER},
                     },
             }) {
            if (m.query.empty()) {
                CLAY_TEXT(
                    Clay_String({
                        .length = static_cast<int32_t>(m.placeholder.length()),
                        .chars = m.placeholder.c_str(),
                    }),
                    CLAY_TEXT_CONFIG({
                        .textColor = COLOR_FOREGROUND_3,
                        .fontSize = 14,
                    }));
            } else {
                CLAY_TEXT(Clay_String({
                              .length = static_cast<int32_t>(m.query.length()),
                              .chars = m.query.c_str(),
                          }),
                          CLAY_TEXT_CONFIG({
                              .textColor = COLOR_FOREGROUND_1,
                              .fontSize = 14,
                          }));
            }
        }
    }

    return m;
}
