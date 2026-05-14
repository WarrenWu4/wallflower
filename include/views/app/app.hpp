#pragma once
#include "views/gallery/gallery.hpp"
#include "views/search_bar/search_bar.hpp"
#include "core/messages.hpp"
#include <queue>

struct AppModel {
    SearchBarModel searchBarModel;
    GalleryModel galleryModel;
};

AppModel App_Init();
AppModel App_Update(AppModel model, Message message);
void App_View(AppModel m, std::queue<Message>& messageQueue);
