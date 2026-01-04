// custom configuration parser
#pragma once

#include "raylib.h"
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

enum class FitMode { COVER, CONTAIN, TILE, FILL };
const std::unordered_map<FitMode, std::string> fitModeToString({
    {FitMode::COVER, "cover"},
    {FitMode::CONTAIN, "contain"},
    {FitMode::TILE, "tile"},
    {FitMode::FILL, "fill"},
});
const std::unordered_map<std::string, FitMode> stringToFitMode({
    {"cover", FitMode::COVER},
    {"contain", FitMode::CONTAIN},
    {"tile", FitMode::TILE},
    {"fill", FitMode::FILL},
});
const std::vector<std::string> modeToStringUpper({"COVER", "CONTAIN", "TILE",
                                                  "FILL"});

struct WallpaperData {
  std::string path;
  FitMode fitMode;
  std::string monitor;
};

struct WallpaperImage {
  Texture2D image;
  float aspectRatio;
};

class Configuration {
public:
  std::string configurationPath;
  std::unordered_set<std::string> directories;
  std::unordered_map<std::string, WallpaperData> wallpapers;
  std::unordered_map<std::string, WallpaperImage> wallpaperImages;

  Configuration();
  ~Configuration();

  std::string getResourcePath();

  std::vector<std::string>
  getImagesFromDirectories(std::vector<std::string> paths);
  void addDirectory(std::string path);
  void removeDirectory(std::string path);

  void parseConfiguration();
  void scanDirectories();
  void loadWallpapers(std::vector<std::string> paths);
  void unloadWallpapers(std::vector<std::string> paths);
  void updateConfiguration();

  void addWallpapers(std::vector<WallpaperData> wallpapers);
};
