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
  for (auto it = configuration->directories.begin(); it != configuration->directories.end(); it++) {
    scanDirectory(*it);
  }
  this->activeWallpaper = "";
  this->wallpapersOrdered = {};
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
    // distribute wallpapers
    wallpapersOrdered = {};
    if (activeWallpaper != "") {
      wallpapersOrdered.push_back(activeWallpaper);
    }
    for (auto it = wallpapers.begin(); it != wallpapers.end(); it++) {
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
        wallpaperEl(i, path, &wallpapers.at(path).imageData, wallpapers.at(path).aspectRatio);
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
      if (configuration->imageData.find(path) != configuration->imageData.end()) {
        runHyprCommand("", path, configuration->imageData.at(path));
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
      if (configuration->imageData.find(path) != configuration->imageData.end()) {
        displayStr = Clay_String({
          .length = static_cast<int32_t>(modeToStringUpper.at(static_cast<int>(configuration->imageData.at(path))).size()),
          .chars = modeToStringUpper.at(static_cast<int>(configuration->imageData.at(path))).c_str()
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

