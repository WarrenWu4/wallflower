#include "settings.hpp"
#include "raylib.h"

Settings::Settings() {
  folderIcon = LoadTexture("resources/icons/folder-icon.png");
  // scan settings folder
}

Settings::~Settings() {
  UnloadTexture(folderIcon);
}

void Settings::addDirectory(std::string directory) {
  directories.insert(directory);
}

void Settings::removeDirectory(std::string directory) {
  directories.erase(directory);
}

void Settings::addFolderButtonEl() {

}

void Settings::settingsContainerEl() {
  CLAY(CLAY_ID("SettingsContainer"), {
    .layout = {
      .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
      .layoutDirection = CLAY_TOP_TO_BOTTOM
    },
  }) {
    CLAY(CLAY_ID("FolderContainer"), {
      .layout = {
        .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
        .childGap = 8,
        .layoutDirection = CLAY_TOP_TO_BOTTOM
      },
    }) {
      std::string dirToRemove = "";
      int id = 0;
      for(auto it = directories.begin(); it != directories.end(); it++, id++) {
        folderEl(id, *it, dirToRemove);
      }
      if (dirToRemove != "") {
        this->removeDirectory(dirToRemove);
      }
    }
  }
}

void Settings::folderEl(int id, const std::string& directory, std::string& dirToRemove) {
  CLAY(CLAY_IDI("Folder", id), {
    .layout = {
      .sizing = {CLAY_SIZING_FIT(), CLAY_SIZING_FIT()},
      .padding = {12, 12, 8, 8},
      .childGap = 8,
      .childAlignment = { .x = CLAY_ALIGN_X_CENTER, .y = CLAY_ALIGN_Y_CENTER }
    },
    .border {
      .color = COLOR_FOREGROUND_1,
      .width = {2, 2, 2, 2, 0}
    },
  }) {
    if (Clay_Hovered() && IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
      dirToRemove = directory;
    }
    CLAY(CLAY_IDI("FolderIcon", id), {
      .layout = {
        .sizing = { .width = CLAY_SIZING_FIXED(16), .height = CLAY_SIZING_FIXED(16) }
      },
      .image = { .imageData = &folderIcon }
    }) {}
    CLAY_TEXT(
      Clay_String({
        .length = static_cast<int32_t>(directory.size()),
        .chars = directory.c_str() 
      }),
      CLAY_TEXT_CONFIG({
        .textColor = COLOR_FOREGROUND_1,
        .fontSize = 20
      }) 
    );
  }
}
