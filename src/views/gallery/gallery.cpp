#include "views/gallery/gallery.hpp"
#include "core/clay.h"
#include "core/colors.h"
#include "core/messages.hpp"
#include <queue>

GalleryModel Gallery_Init() {
    return {
        .activeWallpapers = {},
        .availableWallpapers = {},
    };
}

GalleryModel Gallery_Update(GalleryModel model, GalleryMessageGroup message) {
    std::visit([&](auto&& arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, GalleryMessages::SetWallpaper>) {
            model.activeWallpapers.push_back(arg.wallpaperPath);
        } else if constexpr (std::is_same_v<T, GalleryMessages::ToggleDropdown>) {
            // TODO: implement dropdown toggle logic
            model.availableWallpapers = {"Wallpaper 1", "Wallpaper 2", "Wallpaper 3"};
        }
    }, message);
    return model;
}

void Gallery_View(GalleryModel model, std::queue<Message> &messageQueue) {
    CLAY(CLAY_ID("GalleryContainer"),
         {
             .layout = {.sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
                        .padding = {CLAY_PADDING_ALL(16)},
                        .childGap = 16,
                        .layoutDirection = CLAY_TOP_TO_BOTTOM},
             .backgroundColor = COLOR_FOREGROUND_3,
         }) {}
}
