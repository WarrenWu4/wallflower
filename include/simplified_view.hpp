#pragma once

#include <string>
#include <vector>
#include "configuration.hpp"
#include "raylib.h"

struct WallpaperListItemData {
  Texture2D preview;
  std::string name;
  std::string path;
};

class SimplifiedView {
private:
  std::vector<WallpaperListItemData> wallpaperListItemData;
  void loadWallpaperListItemData();
  void unloadWallpaperListItemData();
  void WallpaperListEl();

  std::shared_ptr<Configuration> configuration;
public:
  SimplifiedView(std::shared_ptr<Configuration> configuration);
  ~SimplifiedView();

  void SimplifiedViewEl();
};
