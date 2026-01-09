#include "wallpapers.hpp"
#include "configuration.hpp"
#include "utils.hpp"
#include "logger.hpp"
#include <cassert>

Wallpapers::Wallpapers(std::shared_ptr<Configuration> configuration, std::shared_ptr<Settings> settings, std::shared_ptr<Dropdown> dropdown) {
  this->configuration = configuration;
  this->settings = settings;
  this->dropdownFitMode = dropdown;

  this->images = {};
  this->directorySnapshot = {};

  this->wallpapersOrdered = {};
  this->activeWallpaper = "";

  onAddDirectory();
  const std::vector<WallpaperData>& temp = configuration->getConfig().wallpapers;
  for (size_t i = 0; i < temp.size(); i++) {
    activeWallpaper = temp.at(i).path;
    loadWallpaper(temp.at(i).path);
  }
}

Wallpapers::~Wallpapers() {
  for (auto it = images.begin(); it != images.end(); it++) {
    UnloadTexture(it->second.image);
  }
}

void Wallpapers::wallpaperContainerEl() {
  CLAY(CLAY_ID("WallpaperContainer"), {
    .layout = {
    .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
    .childGap = 16,
    },
  }) {
    // distribute wallpapers
    wallpapersOrdered = {};
    if (activeWallpaper != "" && images.find(activeWallpaper) != images.end()) {
      wallpapersOrdered.push_back(activeWallpaper);
    }
    for (auto it = images.begin(); it != images.end(); it++) {
      if ((*it).first != activeWallpaper) {
        wallpapersOrdered.push_back((*it).first);
      }
    }
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
    for (size_t i = 0; i < wallpapersOrdered.size(); i++) {
      const std::string& path = wallpapersOrdered.at(i);
      if (i % 3 == col) {
        wallpaperEl(i, path, &images.at(path).image, images.at(path).aspectRatio);
      }
    }
  }
}

void Wallpapers::wallpaperEl(int id, const std::string& path, Texture2D* imageData, float aspectRatio) {
  // Logger::logMsg(LogLabel::DEBUG, std::to_string(id) + ": " + path);
  CLAY(CLAY_IDI("Wallpaper", id), {
    .layout = { 
      .sizing = { .width = CLAY_SIZING_GROW() },
      .childAlignment = {
        .x = CLAY_ALIGN_X_LEFT,
        .y = CLAY_ALIGN_Y_BOTTOM
      }
    }, 
    .aspectRatio = { aspectRatio },
    .image = { .imageData = imageData},
    .border = {
      .color = COLOR_GREEN_DARK,
      .width = (activeWallpaper == path) ? (Clay_BorderWidth) { 2, 2, 2, 2 } : (Clay_BorderWidth) { 0, 0, 0, 0 }
    },
  }) {
    if (Clay_Hovered() && IsMouseButtonPressed(MOUSE_LEFT_BUTTON) && !Clay_PointerOver(Clay_GetElementIdWithIndex(CLAY_STRING("WallpaperMode"), id))) {
      const WallflowerConfig& temp = configuration->getConfig();
      if (temp.preferences.find(path) != temp.preferences.end()) {
        configuration->updateWallpaper("", path, temp.preferences.at(path).fitMode);
      } else {
        configuration->updateWallpaper("", path, settings->defaultMode);
      }
      activeWallpaper = path;
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
      Clay_String displayStr = Clay_String({
        .length = static_cast<int32_t>(modeToStringUpper.at(static_cast<int>(settings->defaultMode)).size()),
        .chars = modeToStringUpper.at(static_cast<int>(settings->defaultMode)).c_str()
      });
      const WallflowerConfig& temp = configuration->getConfig();
      if (temp.preferences.find(path) != temp.preferences.end()) {
        displayStr = Clay_String({
          .length = static_cast<int32_t>(modeToStringUpper.at(static_cast<int>(temp.preferences.at(path).fitMode)).size()),
          .chars = modeToStringUpper.at(static_cast<int>(temp.preferences.at(path).fitMode)).c_str()
        });
      }
      CLAY_TEXT(
        displayStr,
        CLAY_TEXT_CONFIG({ 
          .textColor = COLOR_FOREGROUND_1,
          .fontSize = 20
        })
      );
    }
  }
}

void Wallpapers::loadWallpaper(const std::string& path) {
  std::filesystem::path p(path);
  if (!std::filesystem::exists(p)) {
    Logger::logMsg(LogLabel::FAIL, "Skipped loading image: " + path + ". Path does not exist.");
    return;
  }
  // INFO: always use generic_string to prevent dupes
  if (images.find(p.generic_string()) == images.end()) {
    Texture2D image = LoadTexture(p.generic_string().c_str());
    float aspectRatio = static_cast<float>(image.width) / image.height;
    images[path] = (WallpaperImage) {
      .image = image,
        .aspectRatio = aspectRatio
    };
  }
  Logger::logMsg(LogLabel::DEBUG, "Loading image: " + path);
}

void Wallpapers::unloadWallpaper(const std::string& path) {
  std::filesystem::path p(path);
  if (images.find(p.generic_string()) != images.end()) {
    UnloadTexture(images[p.generic_string()].image);
    images.erase(p.generic_string());
  }
  Logger::logMsg(LogLabel::DEBUG, "Unloading image: " + path);
}

void Wallpapers::onAddDirectory() {
  const WallflowerConfig& temp = configuration->getConfig();
  for(auto it = temp.directories.begin(); it != temp.directories.end(); it++) {
    std::string directory = *it;
    if (directorySnapshot.find(directory) == directorySnapshot.end()) {
      std::vector<std::string> imagePaths = Utils::getImagesInDirectory(directory);
      for (const std::string& path : imagePaths) {
        loadWallpaper(path);
      }
    }
  }
  directorySnapshot = temp.directories;
}

void Wallpapers::onRemoveDirectory() {
  const WallflowerConfig& temp = configuration->getConfig();
  for(auto it = directorySnapshot.begin(); it != directorySnapshot.end(); it++) {
    std::string directory = *it;
    if (temp.directories.find(directory) == temp.directories.end()) {
      std::vector<std::string> imagePaths = Utils::getImagesInDirectory(directory);
      for (const std::string& path : imagePaths) {
        unloadWallpaper(path);
      }
    }
  }
  directorySnapshot = temp.directories;
}

