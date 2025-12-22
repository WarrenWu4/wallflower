#include "wallpapers.hpp"

std::string Wallpapers::uppercaseString(const std::string& str) {
  std::string new_str = "";
  for (const char& c : str) {
    new_str += std::toupper(c);
  }
  return new_str;
}

Wallpapers::Wallpapers(std::shared_ptr<Configuration> configuration, std::shared_ptr<Settings> settings, std::shared_ptr<Dropdown> dropdown) {
  this->configuration = configuration;
  this->settings = settings;
  this->dropdownFitMode = dropdown;
  this->activeWallpaper = "";
  this->wallpapersOrdered = {};
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
    if (activeWallpaper != "") {
      wallpapersOrdered.push_back(activeWallpaper);
    }
    for (auto it = configuration->wallpapers.begin(); it != configuration->wallpapers.end(); it++) {
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
        wallpaperEl(i, path, &configuration->wallpaperImages.at(path).image, configuration->wallpaperImages.at(path).aspectRatio);
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
      .width = (this->activeWallpaper == path) ? (Clay_BorderWidth) { 2, 2, 2, 2 } : (Clay_BorderWidth) { 0, 0, 0, 0 }
    },
  }) {
    if (Clay_Hovered() && IsMouseButtonPressed(MOUSE_LEFT_BUTTON) && !Clay_PointerOver(Clay_GetElementIdWithIndex(CLAY_STRING("WallpaperMode"), id))) {
      if (configuration->wallpapers.find(path) != configuration->wallpapers.end()) {
        runHyprCommand("", path, configuration->wallpapers.at(path).fitMode);
      } else {
        runHyprCommand("", path, settings->defaultMode);
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
      if (configuration->wallpapers.find(path) != configuration->wallpapers.end()) {
        displayStr = Clay_String({
          .length = static_cast<int32_t>(modeToStringUpper.at(static_cast<int>(configuration->wallpapers.at(path).fitMode)).size()),
          .chars = modeToStringUpper.at(static_cast<int>(configuration->wallpapers.at(path).fitMode)).c_str()
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

