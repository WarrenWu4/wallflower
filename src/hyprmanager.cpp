// hypr utility helper functions and classes

#include "hyprmanager.hpp"
#include "configuration.hpp"
#include "logger.hpp"
#include "utils.hpp"
#include <filesystem>
#include <fstream>
#include <sstream>

// FIX: issue where if wallpaper mode isn't specified it defaults to the
// previous mode and since cover mode can't be specified it's stuck in the 2
// other modes
void HyprpaperParser::runHyprCommand(std::string display,
                                     std::string wallpaperPath, FitMode mode) {
  // WARNING: version 0.7.6-4 on arch linux does not support fill and cover is
  // default which must be omitted introduces slight problem where if fit mode
  // is not cover, the new fit mode because the default and since cover is not a
  // keyword that is parsed, it can never return to cover until it is unloaded
  std::string fitMode = fitModeToString.at(mode) + ":";
  if (mode == FitMode::COVER || mode == FitMode::FILL) {
    fitMode = "";
  }
  display += ",";
  std::string unloadCmd = "hyprctl hyprpaper unload \"" + wallpaperPath + "\"";
  std::string preloadCmd =
      "hyprctl hyprpaper preload \"" + wallpaperPath + "\"";
  std::string wallpaperCmd = "hyprctl hyprpaper wallpaper \"" + display +
                             fitMode + wallpaperPath + "\"";
  // FIX: fix system commands since it's dependent on hyprpaper version BRUH
  Logger::logMsg(LogLabel::DEBUG, "Running unload command: " + unloadCmd);
  Logger::logMsg(LogLabel::DEBUG, "Running preload command: " + preloadCmd);
  Logger::logMsg(LogLabel::DEBUG, "Running wallpaper command: " + wallpaperCmd);
  std::system(unloadCmd.c_str());
  std::system(preloadCmd.c_str());
  std::system(wallpaperCmd.c_str());
  this->activeWallpaper = wallpaperPath;
  this->writeConfigToFile();
}

void HyprpaperParser::parsePreload(const std::string &path) {
  // according to hyprpar docs, path must begin with ~ or absolute
  if (path[0] != '/' && path[0] != '~') {
    throw std::runtime_error("Invalid preload path: " + path);
  }
  // check that the path exists
  std::filesystem::path p(path);
  if (!std::filesystem::exists(p)) {
    throw std::runtime_error("Preload path does not exist: " + path);
  }
  preload.push_back(path);
}

void HyprpaperParser::parseWallpaper(const std::string &value) {
  // wallpaper format: monitorName(optional),mode(optional):imagePath
  std::vector<std::string> parts = Utils::split(value, ',');
  std::vector<std::string> modeAndPath =
      Utils::split(parts[parts.size() - 1], ':');
  WallpaperData wp;
  if (parts.size() == 2) {
    wp.monitor = parts[0];
  } else {
    wp.monitor = "";
  }
  wp.path = modeAndPath[modeAndPath.size() - 1];
  if (modeAndPath.size() == 1) {
    wp.fitMode = FitMode::COVER;
  } else {
    if (modeAndPath.at(1) == "contain") {
      wp.fitMode = FitMode::CONTAIN;
    } else if (modeAndPath.at(1) == "tile") {
      wp.fitMode = FitMode::TILE;
    } else {
      wp.fitMode = FitMode::COVER;
    }
  }

  // image path must be in preloaded
  bool preloaded = false;
  for (const auto &preloadPath : preload) {
    if (preloadPath == wp.path) {
      preloaded = true;
      break;
    }
  }
  if (!preloaded) {
    throw std::runtime_error("Wallpaper image path not preloaded: " + wp.path);
  }
  configuration->addWallpapers({wp});
  if (activeWallpaper == "") {
    activeWallpaper = wp.path;
  }
}

HyprpaperParser::HyprpaperParser(std::shared_ptr<Configuration> configuration) {
  configPath = std::string(getenv("HOME")) + "/.config/hypr/hyprpaper.conf";
  this->configuration = configuration;
  preload = {};
  splash = false;
  splash_offset = 2.0f;
  splash_color = "55ffffff";
  ipc = true;
  activeWallpaper = "";
}

void HyprpaperParser::parseFile() {
  std::ifstream file(configPath);
  if (!file.is_open()) {
    throw std::runtime_error("Could not open file: " + configPath);
  }

  std::string line;
  while (std::getline(file, line)) {
    std::string cleanedLine = Utils::removeWhitespace(line);
    std::vector<std::string> values = Utils::split(cleanedLine, '=');
    if (values.size() != 2) {
      throw std::runtime_error("Invalid line in config: " + line);
    }
    std::string key = values[0];
    std::string value = values[1];
    if (key == "preload") {
      parsePreload(value);
    } else if (key == "wallpaper") {
      parseWallpaper(value);
    } else if (key == "splash") {
      splash = (value == "true");
    } else if (key == "splash_offset") {
      splash_offset = std::stof(value);
    } else if (key == "splash_color") {
      splash_color = value;
    } else if (key == "ipc") {
      ipc = (value == "true");
    } else {
      throw std::runtime_error("Unknown config key: " + key);
    }
  }
  file.close();
}

void HyprpaperParser::writeConfigToFile() {
  // create .bak file for old config
  std::ofstream backupFile(configPath + ".bak");
  std::ifstream originalFile(configPath);
  if (!originalFile.is_open()) {
    throw std::runtime_error("Could not open file: " + configPath);
  }
  backupFile << originalFile.rdbuf();
  backupFile.close();
  originalFile.close();

  // update existing config file
  std::ofstream file(configPath);
  file << "preload = " << activeWallpaper << "\n";
  WallpaperData wp = configuration->wallpapers.at(activeWallpaper);
  file << "wallpaper = " << wp.monitor << ", ";
  switch (wp.fitMode) {
  case FitMode::COVER:
    file << "";
    break;
  case FitMode::CONTAIN:
    file << "contain:";
    break;
  case FitMode::TILE:
    file << "tile:";
    break;
  case FitMode::FILL:
    file << "fill:";
    break;
  }
  file << wp.path << "\n";
  file << "splash = " << (splash ? "true" : "false") << "\n";
  file << "splash_offset = " << splash_offset << "\n";
  file << "splash_color = " << splash_color << "\n";
  file << "ipc = " << (ipc ? "true" : "false") << "\n";
  file.close();
}
