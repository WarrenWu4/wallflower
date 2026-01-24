#pragma once

#include "configuration.hpp"
#include "raylib.h"
#include <memory>

class WallpaperDropdown {
private:
  std::shared_ptr<Configuration> config;
  bool show;
  Vector2 mousePosition;
  
  Texture2D fitModeIcon;
  Texture2D monitorIcon;
  Texture2D radioButton;
  Texture2D radioButtonActive;

  std::string defaultMonitor;

  void callback(); 

public:
  // path of the wallpaper the dropdown is configuring
  std::string wallpaperPath;

  WallpaperDropdown(std::shared_ptr<Configuration> _config);
  ~WallpaperDropdown();

  void open();
  void close();
  void toggle();

  void dropdownEl();
  void fitModeEl();
  void fitModeOptionEl(int id, const std::string& fitMode, bool selected);
  void monitorsEl();
  void monitorOptionEl(int id, const std::string& name, bool selected);
};
