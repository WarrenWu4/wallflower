#include "wallpapers.hpp"
#include "raylib.h"

Wallpapers::Wallpapers(std::shared_ptr<Configuration> configuration, std::shared_ptr<Settings> settings, std::shared_ptr<Dropdown> dropdown) {
  this->configuration = configuration;
  this->settings = settings;
  this->dropdownFitMode = dropdown;
  for (auto it = configuration->directories.begin(); it != configuration->directories.end(); it++) {
    scanDirectory(*it);
  }
  // TODO: implement active wallpaper to front
  activeWallpaper = "";
}

Wallpapers::~Wallpapers() {
  for (auto it = wallpapers.begin(); it != wallpapers.end(); it++) {
    UnloadTexture(it->second.imageData);
  }
}

void Wallpapers::addWallpaper(std::string path) {
  Texture2D imageData = LoadTexture(path.c_str());
  float aspectRatio = static_cast<float>(imageData.width) / imageData.height;
  wallpapers[path] = { imageData, aspectRatio };
}

void Wallpapers::removeWallpaper(std::string path) {
  UnloadTexture(wallpapers[path].imageData);
  wallpapers.erase(path);
}

void Wallpapers::scanDirectory(std::string path) {
  std::filesystem::path p(path);
  if (std::filesystem::exists(p) && std::filesystem::is_directory(p)) {
    for (const auto& entry : std::filesystem::directory_iterator(p)) {
      if (entry.is_regular_file()) {
        std::string ext = entry.path().extension().string();
        std::transform(ext.begin(), ext.end(), ext.begin(), ::tolower);
        if (ext == ".png" || ext == ".jpg" || ext == ".jpeg") {
          addWallpaper(entry.path());
        }
      }
    }
  } else {
    std::cerr << "Path is not a directory or does not exist\n";
  }
}

void Wallpapers::wallpaperContainerEl() {
  CLAY(CLAY_ID("WallpaperContainer"), {
    .layout = {
    .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
    .childGap = 16,
    },
  }) {
    wallpaperColEl(0);
    wallpaperColEl(1);
    wallpaperColEl(2);
  }
}

void Wallpapers::wallpaperColEl(int col) {
  CLAY(CLAY_IDI("WallpaperCol", col), {
    .layout = { 
      .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0) }, 
      .childGap = 16,
      .layoutDirection = CLAY_TOP_TO_BOTTOM
    }
  }) {
    int id = 0;
    for(auto it = wallpapers.begin(); it != wallpapers.end(); it++, id++) {
      if (id % 3 == col) {
        wallpaperEl(id, it->first, &it->second.imageData, it->second.aspectRatio);
      }
    }
  }
}

void Wallpapers::wallpaperEl(int id, const std::string& path, Texture2D* imageData, float aspectRatio) {
  CLAY(CLAY_IDI("Wallpaper", id), {
    .layout = { 
      .sizing = { .width = CLAY_SIZING_GROW() }
    }, 
    .aspectRatio = { aspectRatio },
    .image = { .imageData = imageData} 
  }) {
    if (Clay_Hovered() && IsMouseButtonPressed(MOUSE_LEFT_BUTTON) && !Clay_PointerOver(Clay_GetElementIdWithIndex(CLAY_STRING("WallpaperMode"), id))) {
      if (configuration->imageData.find(path) != configuration->imageData.end()) {
        runHyprCommand("", path, configuration->imageData.at(path));
      } else {
        runHyprCommand("", path, settings->defaultMode);
      }
    }
    CLAY(CLAY_IDI("WallpaperMode", id), {
      .layout = {
        .sizing = { .width = CLAY_SIZING_FIT(), .height = CLAY_SIZING_FIT() },
        .padding = {12, 12, 8, 8}
      },
      .backgroundColor = COLOR_BACKGROUND_0,
    }) {
      if (Clay_Hovered() && IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
        dropdownFitMode->show = true;
        dropdownFitMode->parentName = "WallpaperMode";
        dropdownFitMode->parentId = id;
        dropdownFitMode->data = &const_cast<std::string&>(path);
      }
      CLAY_TEXT(
        CLAY_STRING("COVER"),
        CLAY_TEXT_CONFIG({ 
          .textColor = COLOR_FOREGROUND_1,
          .fontSize = 20
        })
      );
    }
  }
}

