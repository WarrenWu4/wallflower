// custom configuration parser
#pragma once

#include "hyprmanager.hpp"
#include <unordered_set>
#include <string>
#include <unordered_map>
#include <iostream>
#include <fstream>
#include <sstream>

class Configuration {
private:
  void printConfiguration();

public:
  std::string configurationPath;
  std::unordered_set<std::string> directories;
  std::unordered_map<std::string, WallpaperMode> imageData;

  Configuration();
  ~Configuration();

  // WARNING: should only be ran once when Configuration is initialized to avoid heavy reading from disk
  void parseConfiguration();
  // WARNING: should only run at application close to avoid weird behavior with writes and performance
  void updateConfiguration();
};
