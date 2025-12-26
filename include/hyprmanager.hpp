#pragma once

#include "configuration.hpp"
#include <memory>
#include <string>
#include <vector>

class HyprpaperParser {
private:
  std::shared_ptr<Configuration> configuration;
  std::vector<std::string> preload;
  bool splash;
  float splash_offset;
  std::string splash_color;
  bool ipc;
  std::string configPath;

  void parsePreload(const std::string &path);
  void parseWallpaper(const std::string &value);

public:
  // first wallpaper in hyprpaper.conf
  std::string activeWallpaper;

  void runHyprCommand(std::string display, std::string wallpaperPath,
                      FitMode mode);

  HyprpaperParser(std::shared_ptr<Configuration> configuration);

  void parseFile();
  void writeConfigToFile();
};
