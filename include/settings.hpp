#pragma once

#include "clay.h"
#include "colors.h"
#include "raylib.h"
#include <string>
#include <unordered_set>
#include <iostream>
#include <memory>

class Settings {
public:
  std::unordered_set<std::string> directories;
  Texture2D folderIcon;

  Settings();
  ~Settings();

  void addDirectory(std::string directory);
  void removeDirectory(std::string directory);

  void settingsContainerEl();
  void addFolderButtonEl(); 
  void folderEl(int id, const std::string& directory, std::string& dirToRemove);
};
