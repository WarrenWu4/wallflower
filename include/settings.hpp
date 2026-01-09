#pragma once

#include "clay.h"
#include "configuration.hpp"
#include "raylib.h"
#include <string>
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
