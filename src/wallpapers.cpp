#include "wallpapers.hpp"
#include "configuration.hpp"
#include "colors.h"
#include "logger.hpp"
#include "raylib.h"
#include "utils.hpp"
#include "wallpaper_dropdown.hpp"
#include <cassert>

Wallpapers::Wallpapers(std::shared_ptr<Configuration> configuration,
                       std::shared_ptr<Settings> settings,
                       std::shared_ptr<WallpaperDropdown> wd) {
  this->configuration = configuration;
  this->settings = settings;
  this->dropdown = wd;

  this->images = {};
  this->searchPathSnapshot = {};

  this->wallpapersOrdered = {};
  this->activeWallpaper = "";

  onSearchPathChange(true);

  const std::vector<WallpaperData> &temp =
      configuration->getConfig().wallpapers;
  for (size_t i = 0; i < temp.size(); i++) {
    activeWallpaper = temp.at(i).path;
    loadWallpaper(temp.at(i).path);
  }
}

Wallpapers::~Wallpapers() {
  for (auto it = images.begin(); it != images.end(); it++) {
    UnloadTexture(it->second.image);
  }
  Logger::logMsg(LogLabel::OK, "Destructor ran");
}

void Wallpapers::wallpaperContainerEl() {
  CLAY(CLAY_ID("WallpaperContainer"),
       {
           .layout =
               {
                   .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
                   .childGap = 16,
                   .layoutDirection = CLAY_TOP_TO_BOTTOM
               },
       }) {
    CLAY_TEXT(
      CLAY_STRING("Left click to update wallpaper"),
      CLAY_TEXT_CONFIG({
        .textColor = COLOR_FOREGROUND_3,
        .fontSize = 20
      })
    );
    CLAY_TEXT(
      CLAY_STRING("Right click to configure wallpaper"),
      CLAY_TEXT_CONFIG({
        .textColor = COLOR_FOREGROUND_3,
        .fontSize = 20
      })
    );
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
    CLAY(CLAY_ID("WallpaperColContainer"), {
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
}

void Wallpapers::wallpaperColEl(int col) {
  CLAY(CLAY_IDI("WallpaperCol", col),
       {.layout = {.sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
                   .childGap = 16,
                   .layoutDirection = CLAY_TOP_TO_BOTTOM}}) {
    for (size_t i = 0; i < wallpapersOrdered.size(); i++) {
      const std::string &path = wallpapersOrdered.at(i);
      if (static_cast<int>(i) % 3 == col) {
        wallpaperEl(i, path, &images.at(path).image,
                    images.at(path).aspectRatio);
      }
    }
  }
}

void Wallpapers::wallpaperEl(int id, const std::string &path,
                             Texture2D *imageData, float aspectRatio) {
  // Logger::logMsg(LogLabel::DEBUG, std::to_string(id) + ": " + path);
  CLAY(CLAY_IDI("Wallpaper", id),
       {
           .layout = {.sizing = {.width = CLAY_SIZING_GROW()},
                      .childAlignment = {.x = CLAY_ALIGN_X_LEFT,
                                         .y = CLAY_ALIGN_Y_BOTTOM}},
           .aspectRatio = {aspectRatio},
           .image = {.imageData = imageData},
           .border = {.color = COLOR_GREEN_DARK,
                      .width = (activeWallpaper == path)
                                   ? (Clay_BorderWidth){2, 2, 2, 2}
                                   : (Clay_BorderWidth){0, 0, 0, 0}},
       }) {
    if (Clay_Hovered() && !Clay_PointerOver(Clay_GetElementIdWithIndex(
                              CLAY_STRING("WallpaperMode"), id))) {
      if (IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
        Logger::logMsg(LogLabel::DEBUG, "Updating active wallpaper");
        const WallflowerConfig &temp = configuration->getConfig();
        if (temp.preferences.find(path) != temp.preferences.end()) {
          configuration->updateWallpaper("", path,
                                         temp.preferences.at(path).fitMode);
        } else {
          configuration->updateWallpaper("", path, settings->defaultMode);
        }
        activeWallpaper = path;
      } else if (IsMouseButtonPressed(MOUSE_BUTTON_RIGHT)) {
        Logger::logMsg(LogLabel::DEBUG, "Opening wallpaper config menu");
        dropdown->open();
      }
    }
  }
}

void Wallpapers::loadWallpaper(const std::string &path) {
  std::filesystem::path p(path);
  if (!std::filesystem::exists(p)) {
    Logger::logMsg(LogLabel::FAIL,
                   "Skipped loading image: " + path + ". Path does not exist.");
    return;
  }
  // INFO: always use generic_string to prevent dupes
  if (images.find(p.generic_string()) == images.end()) {
    Texture2D image = LoadTexture(p.generic_string().c_str());
    float aspectRatio = static_cast<float>(image.width) / image.height;
    images[path] = (WallpaperImage){.image = image, .aspectRatio = aspectRatio};
  }
  Logger::logMsg(LogLabel::DEBUG, "Loading image: " + path);
}

void Wallpapers::unloadWallpaper(const std::string &path) {
  std::filesystem::path p(path);
  if (images.find(p.generic_string()) != images.end()) {
    UnloadTexture(images[p.generic_string()].image);
    images.erase(p.generic_string());
  }
  Logger::logMsg(LogLabel::DEBUG, "Unloading image: " + path);
}

void Wallpapers::onSearchPathChange(bool signalType) {
  const WallflowerConfig &temp = configuration->getConfig();
  if (signalType) {
    for (auto it = temp.searchPaths.begin(); it != temp.searchPaths.end();
         it++) {
      std::string searchPath = *it;
      if (searchPathSnapshot.find(searchPath) == searchPathSnapshot.end()) {
        if (std::filesystem::is_directory(searchPath)) {
          std::vector<std::string> imagePaths =
              Utils::getImagesInDirectory(searchPath);
          for (const std::string &path : imagePaths) {
            loadWallpaper(path);
          }
        } else {
          loadWallpaper(searchPath);
        }
      }
    }
  } else {
    const WallflowerConfig &temp = configuration->getConfig();
    for (auto it = searchPathSnapshot.begin(); it != searchPathSnapshot.end();
         it++) {
      std::string searchPath = *it;
      if (temp.searchPaths.find(searchPath) == temp.searchPaths.end()) {
        if (std::filesystem::is_directory(searchPath)) {
          std::vector<std::string> imagePaths =
              Utils::getImagesInDirectory(searchPath);
          for (const std::string &path : imagePaths) {
            unloadWallpaper(path);
          }
        } else {
          unloadWallpaper(searchPath);
        }
      }
    }
  }
  searchPathSnapshot = temp.searchPaths;
}
