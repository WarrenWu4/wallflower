#pragma once
#include <string>
#include <vector>

// event/message definitions
typedef enum {
    MSG_SET_WALLPAPER,
    MSG_TOGGLE_DROPDOWN,
} GalleryMsg;

// define the app data
typedef struct {
    std::vector<std::string> activeWallpapers;
    std::vector<std::string> availableWallpapers;
} GalleryModel;

// view functions (update to handle events and view to render UI)
GalleryModel Gallery_Init();
GalleryModel Gallery_Update(GalleryModel m, GalleryMsg msg,
                            std::string payload = "");
GalleryMsg Gallery_View(const GalleryModel *m);
