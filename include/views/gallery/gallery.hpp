#pragma once
#include "core/messages.hpp"
#include <queue>
#include <string>
#include <vector>

struct GalleryModel {
    std::vector<std::string> activeWallpapers;
    std::vector<std::string> availableWallpapers;
};

// view functions (update to handle events and view to render UI)
GalleryModel Gallery_Init();
GalleryModel Gallery_Update(GalleryModel model, GalleryMessageGroup message);
void Gallery_View(GalleryModel model, std::queue<Message> &messageQueue);
