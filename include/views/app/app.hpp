#pragma once
#include "views/gallery/gallery.hpp"
#include "views/search_bar/search_bar.hpp"
#include "core/messages.hpp"
#include <memory>
#include <queue>

struct AppModel {
    std::shared_ptr<SearchBarModel> searchBarModel;
    std::shared_ptr<GalleryModel> galleryModel;
};

std::shared_ptr<AppModel> App_Init();
void App_Update(std::shared_ptr<AppModel> model, Message message);
void App_View(std::shared_ptr<AppModel> m, std::queue<Message>& messageQueue);
