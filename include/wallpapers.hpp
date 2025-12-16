#pragma once

#include "raylib.h"
#include <string>
#include <unordered_map>
#include <vector>

struct Wallpaper {
  Texture2D imageData;
  float aspectRatio;
};

class Wallpapers {
public:
  std::unordered_map<std::string, Wallpaper> wallpapers;
  std::vector<std::string> directories;

  Wallpapers();
  ~Wallpapers();

  void addWallpaper(std::string path);
  void removeWallpaper(std::string path);

  void scanDirectory(std::string path);

  void wallpaperContainerEl();
  void wallpaperColEl(int col);
  void wallpaperEl(int id, std::string path, Texture2D* imageData, float aspectRatio);
};
