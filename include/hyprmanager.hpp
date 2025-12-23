#pragma once

#include "configuration.hpp"
#include <string>
#include <vector>
#include <memory>

const std::vector<std::string> modeToStringUpper({
  "COVER",
  "CONTAIN",
  "TILE",
  "FILL"
});

void runHyprCommand(std::string display, std::string wallpaperPath, FitMode mode);

class HyprpaperParser {
private:
  std::shared_ptr<Configuration> configuration;
  std::vector<std::string> preload;
  bool splash;
  float splash_offset;
  std::string splash_color;
  bool ipc;
  std::string configPath;
  // first wallpaper in hyprpaper.conf

  std::vector<std::string> split(const std::string &s, char delimiter);
  std::string removeWhitespace(const std::string &s);

  void parsePreload(const std::string &path);
  void parseWallpaper(const std::string &value);

public:
  std::string activeWallpaper;

  HyprpaperParser(std::shared_ptr<Configuration> configuration);

  void parseFile();
  void writeConfigToFile();
};
