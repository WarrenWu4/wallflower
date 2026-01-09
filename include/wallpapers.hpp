#pragma once

#include "clay.h"
#include "dropdown.hpp"
#include "raylib.h"
#include "settings.hpp"
#include <string>
#include <vector>

struct WallpaperImage {
  Texture2D image;
  float aspectRatio;
};

class Wallpapers {
private:
  std::shared_ptr<Configuration> configuration;
  std::shared_ptr<Settings> settings;
  std::shared_ptr<Dropdown> dropdownFitMode;

  std::unordered_map<std::string, WallpaperImage> images;
  std::unordered_set<std::string> directorySnapshot;

  std::vector<std::string> wallpapersOrdered;
  std::string activeWallpaper;

public:
  Wallpapers(std::shared_ptr<Configuration> configuration,
             std::shared_ptr<Settings> settings,
             std::shared_ptr<Dropdown> dropdown);
  ~Wallpapers();

  void wallpaperContainerEl();
  void wallpaperColEl(int col);
  void wallpaperEl(int id, const std::string &path, Texture2D *imageData,
                   float aspectRatio);

  /*
   * helper function that loads wallpaper to raylib
   * and adds the data to 'images' member variable
   * also checks if the path exists that it doesn't already 
   * exist in member variable 'images'
   */
  void loadWallpaper(const std::string& path);

  /*
   * helper function that unloads wallpaper and
   * removes it from 'images' member variable
   * checks if the path is in 'images' member variable
   * prior to the removal
   */
  void unloadWallpaper(const std::string& path);

  /*
   * slot function that scans new directories
   * by comparing with directorySnapshot
   * for each image path it checks if path exists
   * and if the image is new
   */
  void onAddDirectory();
  
  /*
   * slot function that scans new directories
   * by comparing with directorySnapshot
   * for each image path it checks if path exists
   * then unloads the image from memory 
   */
  void onRemoveDirectory();
};
