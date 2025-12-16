#pragma once

#include <string>
#include <vector>

void runHyprCommand(std::string display, std::string wallpaperPath);

enum class WallpaperMode { COVER, CONTAIN, TILE };

struct WallpaperParams {
  std::string monitorName;
  std::string imagePath;
  WallpaperMode mode;
};

struct HyprpaperConfig {
  std::vector<std::string> preload;
  std::vector<WallpaperParams> wallpaper;
  bool splash;
  float splash_offset;
  std::string splash_color;
  bool ipc;
};

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
