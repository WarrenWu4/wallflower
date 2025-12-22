#pragma once

#include "configuration.hpp"
#include <string>
#include <vector>

const std::vector<std::string> modeToStringUpper({
  "COVER",
  "CONTAIN",
  "TILE",
  "FILL"
});

struct HyprpaperConfig {
  std::vector<std::string> preload;
  std::vector<WallpaperData> wallpaper;
  bool splash;
  float splash_offset;
  std::string splash_color;
  bool ipc;
};

void runHyprCommand(std::string display, std::string wallpaperPath, FitMode mode);

class HyprpaperParser {
private:
  HyprpaperConfig config;
  std::string configPath;

  std::vector<std::string> split(const std::string &s, char delimiter);
  std::string removeWhitespace(const std::string &s);

  void parsePreload(const std::string &path);
  void parseWallpaper(const std::string &value);

public:
  HyprpaperParser();

  void parseFile();
  void writeConfigToFile();

  HyprpaperConfig getConfig();
  void setConfig(const HyprpaperConfig &newConfig);
};
