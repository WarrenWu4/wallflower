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

  void callback(); 
public:
  WallpaperDropdown(std::shared_ptr<Configuration> _config);
  ~WallpaperDropdown();

  void open();
  void close();
  void toggle();

  void dropdownEl();
};
