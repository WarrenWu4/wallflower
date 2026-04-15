#pragma once

#include "../core/clay.h"
#include "configuration.hpp"
#include "../core/raylib.h"
#include <memory>
#include <string>

const std::vector<std::string> supportedImageFormats = {
  ".png",
  ".jpg",
  ".jpeg"
};

class Settings {
private:
  std::string searchPathToRemove;
public:
  std::shared_ptr<Configuration> configuration;
  Texture2D folderIcon;
  Texture2D fileIcon;
  FitMode defaultMode;

  Settings(std::shared_ptr<Configuration> configuration);
  ~Settings();

  void folderPicker();
  void imagePicker();

  void settingsContainerEl();
  void addFolderButtonEl();
  void addImageButtonEl();
  void folderEl(int id, const std::string &directory, std::string &dirToRemove);
};
