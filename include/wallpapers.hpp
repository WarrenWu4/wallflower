#pragma once

#include "clay.h"
#include "raylib.h"
#include "dropdown.hpp"
#include "settings.hpp"
#include <string>
#include <vector>

class Wallpapers {
private:
  std::string uppercaseString(const std::string& str);
public:
  std::shared_ptr<Configuration> configuration;
  std::shared_ptr<Settings> settings;
  std::shared_ptr<Dropdown> dropdownFitMode;

  std::vector<std::string> wallpapersOrdered;
  std::string activeWallpaper;

  Wallpapers(std::shared_ptr<Configuration> configuration, std::shared_ptr<Settings> settings, std::shared_ptr<Dropdown> dropdown);

  void wallpaperContainerEl();
  void wallpaperColEl(int col);
  void wallpaperEl(int id, const std::string& path, Texture2D* imageData, float aspectRatio);
};
