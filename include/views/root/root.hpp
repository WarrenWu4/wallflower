#pragma once
#include "views/gallery/gallery.hpp"
#include "views/search_bar/search_bar.hpp"
#include "core/base_msg.hpp"
#include <string>

typedef enum {
    MSG_NONE,
} RootMsg;

typedef struct {
    SearchBarModel searchBarModel;
    GalleryModel galleryModel;
} RootModel;

RootModel Root_Init();
RootModel Root_Update(RootModel m, RootMsg msg, std::string payload = "");
RootModel Root_View(RootModel m);
