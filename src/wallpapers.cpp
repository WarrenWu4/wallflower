#include "clay.h"
#include "raylib.h"
#include "colors.h"
#include "hyprmanager.cpp"
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
  std::unordered_map<std::string, Wallpaper> wallpapers;
  std::vector<std::string> directories;

  Wallpapers() {
    for (std::string directory : directories) {
      scanDirectory(directory);
    }
  }

  ~Wallpapers() {
    for (auto it = wallpapers.begin(); it != wallpapers.end(); it++) {
      UnloadTexture(it->second.imageData);
    }
  }

  void addWallpaper(std::string path) {
    Texture2D imageData = LoadTexture(path.c_str());
    float aspectRatio = static_cast<float>(imageData.width) / imageData.height;
    wallpapers[path] = { imageData, aspectRatio };
  }

  void removeWallpaper(std::string path) {
    UnloadTexture(wallpapers[path].imageData);
    wallpapers.erase(path);
  }

  void scanDirectory(std::string path) {
    std::filesystem::path p(path);
    if (std::filesystem::exists(p) && std::filesystem::is_directory(p)) {
      for (const auto& entry : std::filesystem::recursive_directory_iterator(p)) {
        if (entry.is_regular_file()) {
          std::string ext = entry.path().extension().string();
          std::transform(ext.begin(), ext.end(), ext.begin(), ::tolower);
          if (ext == ".png" || ext == ".jpg" || ext == ".jpeg") {
            addWallpaper(entry.path());
          }
        }
      }
    } else {
      std::cerr << "Path is not a directory or does not exist\n";
    }
  }

  void wallpaperContainerEl() {
    CLAY(CLAY_ID("WallpaperContainer"), {
      .layout = {
      .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
      .childGap = 16,
      },
      .backgroundColor = COLOR_BACKGROUND_4
    }) {
      wallpaperColEl(0);
      wallpaperColEl(1);
      wallpaperColEl(2);
    }
  }
  
  void wallpaperColEl(int col) {
    CLAY(CLAY_IDI("WallpaperCol", col), {
      .layout = { 
        .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0) }, 
        .childGap = 16,
        .layoutDirection = CLAY_TOP_TO_BOTTOM
      },
      .backgroundColor = (col == 0) ? COLOR_RED_DARK : (col == 1) ? COLOR_BLUE_DARK : COLOR_GREEN_DARK
    }) {
      int id = 0;
      for(auto it = wallpapers.begin(); it != wallpapers.end(); it++, id++) {
        if (id % 3 == col) {
          wallpaperEl(id, it->first, &it->second.imageData, it->second.aspectRatio);
        }
      }
    }
  }
  
  void wallpaperEl(int id, std::string path, Texture2D* imageData, float aspectRatio) {
    CLAY(CLAY_IDI("Wallpaper", id), {
      .layout = { 
        .sizing = { .width = CLAY_SIZING_GROW() }
      }, 
      .aspectRatio = { aspectRatio },
      .image = { .imageData = imageData} 
    }) {
      if (Clay_Hovered() && IsMouseButtonPressed(MOUSE_LEFT_BUTTON)) {
        runHyprCommand(",", path);
      }
    }
  }
};

