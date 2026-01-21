#pragma once

#include "configuration.hpp"
#include "raylib.h"
#include <memory>

class WallpaperDropdown {
private:
  std::shared_ptr<Configuration> config;
  bool show;
  Vector2 mousePosition;
  
  Texture2D monitorIcon;
  Texture2D radioButton;
  Texture2D activeRadioButton;

  std::string defaultMonitor;

  void callback(); 

public:
  // data needed from wallpaper to run updateWallpaper
  std::string wallpaperPath;
  FitMode wallpaperFitMode;

  WallpaperDropdown(std::shared_ptr<Configuration> _config);
  ~WallpaperDropdown();

  void open();
  void close();
  void toggle();

  void dropdownEl();
  void monitorOptionEl(int id, const std::string& name, bool selected);
};
