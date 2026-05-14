#pragma once
#include "core/messages.hpp"
#include <memory>
#include <queue>
#include <string>
#include <vector>

struct GalleryModel {
    std::vector<std::string> activeWallpapers;
    std::vector<std::string> availableWallpapers;
};

// view functions (update to handle events and view to render UI)
std::shared_ptr<GalleryModel> Gallery_Init();
void Gallery_Update(std::shared_ptr<GalleryModel> model, GalleryMessageGroup message);
void Gallery_View(std::shared_ptr<GalleryModel> model, std::queue<Message> &messageQueue);
