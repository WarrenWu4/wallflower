#include "views/app/app.hpp"
#include "core/clay.h"
#include "core/colors.h"

AppModel App_Init() {
    return {
        .searchBarModel = SearchBar_Init(),
        .galleryModel = Gallery_Init(),
    };
}

AppModel App_Update(AppModel m, Message msg) {
    std::visit([&](auto&& arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, SearchBarMessageGroup>) {
            m.searchBarModel =
                SearchBar_Update(m.searchBarModel, arg);
        }
    }, msg);
    return m;
}

void App_View(AppModel m, std::queue<Message>& messageQueue) {
    CLAY(CLAY_ID("AppContainer"),
         {
             .layout = {.sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
                        .padding = {CLAY_PADDING_ALL(16)},
                        .childGap = 16,
                        .layoutDirection = CLAY_TOP_TO_BOTTOM},
             .backgroundColor = COLOR_BACKGROUND_1,
         }) {
        SearchBar_View(m.searchBarModel, messageQueue);
        Gallery_View(m.galleryModel, messageQueue);
    }
}
