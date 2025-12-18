#pragma once

#include "clay.h"
#include "raylib.h"
#include "dropdown.hpp"
#include "settings.hpp"
#include <string>
#include <unordered_map>
#include <vector>
#include <filesystem>
#include <algorithm>
#include <iostream>

struct Wallpaper {
  Texture2D imageData;
  float aspectRatio;
};

class Wallpapers {
public:
  std::shared_ptr<Configuration> configuration;
  std::shared_ptr<Settings> settings;
  std::shared_ptr<Dropdown> dropdownFitMode;
  std::unordered_map<std::string, Wallpaper> wallpapers;
  std::string activeWallpaper;

  Wallpapers(std::shared_ptr<Configuration> configuration, std::shared_ptr<Settings> settings, std::shared_ptr<Dropdown> dropdown);
  ~Wallpapers();

  void addWallpaper(std::string path);
  void removeWallpaper(std::string path);

  void scanDirectory(std::string path);

  void wallpaperContainerEl();
  void wallpaperColEl(int col);
  void wallpaperEl(int id, std::string path, Texture2D* imageData, float aspectRatio);
};
