#pragma once

#include "clay.h"
#include "colors.h"
#include "configuration.hpp"
#include "raylib.h"
#include "hyprmanager.hpp"
#include <string>
#include <unordered_set>
#include <iostream>
#include <memory>

class Settings {
public:
  std::shared_ptr<Configuration> configuration;
  Texture2D folderIcon;
  FitMode defaultMode;

  Settings(std::shared_ptr<Configuration> configuration);
  ~Settings();

  void folderPicker();

  void settingsContainerEl();
  void addFolderButtonEl(); 
  void folderEl(int id, const std::string& directory, std::string& dirToRemove);
};
