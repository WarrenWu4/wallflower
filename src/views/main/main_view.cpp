#include "main.hpp"
#include "../../include/../../core/clay.h"
#include "../../include/../../core/colors.h"

AppMsg Main_View(const AppModel *m) {
    AppMsg result = MSG_NONE;

    CLAY(CLAY_ID("MainContainer"), {
        .layout = {
            .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
            .padding = {CLAY_PADDING_ALL(16)},
            .childGap = 16,
            .layoutDirection = CLAY_TOP_TO_BOTTOM
        },
        .backgroundColor = COLOR_BACKGROUND_1,
    }) {
        CLAY(CLAY_ID("ActionBarPlaceholder"), {
            .layout = { .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_FIXED(40)} },
            .backgroundColor = COLOR_BACKGROUND_2
        }) {}
    }

    return result;
}
