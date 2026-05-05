#pragma once
#include "views/gallery/gallery.hpp"
#include <string>

// event/message definitions
typedef enum {
    MSG_NONE,
    MSG_SELECT_TAB,
} AppMsg;

typedef enum { TAB_GALLERY, TAB_SETTINGS } TabType;

// define the app data
typedef struct {
    TabType currentTab;
    GalleryModel galleryModel;
} AppModel;

// view functions (update to handle events and view to render UI)
AppModel Main_Update(AppModel m, AppMsg msg, std::string payload = "");
AppMsg Main_View(const AppModel *m);
