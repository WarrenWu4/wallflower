// hypr utility helper functions and classes

#include "hyprmanager.hpp"

// FIX: issue where if wallpaper mode isn't specified it defaults to the previous mode
// and since cover mode can't be specified it's stuck in the 2 other modes
void runHyprCommand(std::string display, std::string wallpaperPath, WallpaperMode mode) {
  // WARNING: version 0.7.6-4 on arch linux does not support fill and cover is default which must be omitted
  // introduces slight problem where if fit mode is not cover, the new fit mode because the default
  // and since cover is not a keyword that is parsed, it can never return to cover until it is unloaded
  std::string fitMode = modeToString.at(static_cast<int>(mode)) + ":";
  if (mode == WallpaperMode::COVER || mode == WallpaperMode::FILL) {
    fitMode = "";
  }
  display += ",";
  std::string unloadCmd = "hyprctl hyprpaper unload \"" + wallpaperPath + "\"";
  std::string preloadCmd = "hyprctl hyprpaper preload \"" + wallpaperPath + "\"";
  std::string wallpaperCmd =
      "hyprctl hyprpaper wallpaper \"" + display + fitMode + wallpaperPath + "\"";
  // FIX: fix system commands since it's dependent on hyprpaper version BRUH
  Logger::logMsg(LogLabel::DEBUG, "Running unload command: " + unloadCmd);
  Logger::logMsg(LogLabel::DEBUG, "Running preload command: " + preloadCmd);
  Logger::logMsg(LogLabel::DEBUG, "Running wallpaper command: " + wallpaperCmd);
  std::system(unloadCmd.c_str());
  std::system(preloadCmd.c_str());
  std::system(wallpaperCmd.c_str());
}

std::vector<std::string> HyprpaperParser::split(const std::string &s,
                                                char delimiter) {
  std::vector<std::string> tokens;
  std::stringstream ss(s);
  std::string token;
  while (std::getline(ss, token, delimiter)) {
    tokens.push_back(token);
  }
  return tokens;
}

std::string HyprpaperParser::removeWhitespace(const std::string &s) {
  std::string result;
  for (char c : s) {
    if (!isspace(c)) {
      result += c;
    }
  }
  return result;
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
  config.preload.push_back(path);
}

void HyprpaperParser::parseWallpaper(const std::string &value) {
  // wallpaper format: monitorName(optional),mode(optional):imagePath
  std::vector<std::string> parts = split(value, ',');
  std::vector<std::string> modeAndPath = split(parts[parts.size() - 1], ':');
  WallpaperParams wp;
  if (parts.size() == 2) {
    wp.monitorName = parts[0];
  } else {
    wp.monitorName = "";
  }
  wp.imagePath = modeAndPath[modeAndPath.size() - 1];
  if (modeAndPath.size() == 1) {
    wp.mode = WallpaperMode::COVER;
  } else {
    if (modeAndPath.at(1) == "contain") {
      wp.mode = WallpaperMode::CONTAIN;
    } else if (modeAndPath.at(1) == "tile") {
      wp.mode = WallpaperMode::TILE;
    } else {
      wp.mode = WallpaperMode::COVER;
    }
  }

  // image path must be in preloaded
  bool preloaded = false;
  for (const auto &preloadPath : config.preload) {
    if (preloadPath == wp.imagePath) {
      preloaded = true;
      break;
    }
  }
  if (!preloaded) {
    throw std::runtime_error("Wallpaper image path not preloaded: " +
                             wp.imagePath);
  }
  config.wallpaper.push_back(wp);
}

HyprpaperParser::HyprpaperParser() {
  configPath = std::string(getenv("HOME")) + "/.config/hypr/hyprpaper.conf";
  config.preload = {};
  config.wallpaper = {};
  config.splash = false;
  config.splash_offset = 2.0f;
  config.splash_color = "55ffffff";
  config.ipc = true;
}

void HyprpaperParser::parseFile() {
  std::ifstream file(configPath);
  if (!file.is_open()) {
    throw std::runtime_error("Could not open file: " + configPath);
  }

  std::string line;
  while (std::getline(file, line)) {
    std::string cleanedLine = removeWhitespace(line);
    std::vector<std::string> values = split(cleanedLine, '=');
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
      config.splash = (value == "true");
    } else if (key == "splash_offset") {
      config.splash_offset = std::stof(value);
    } else if (key == "splash_color") {
      config.splash_color = value;
    } else if (key == "ipc") {
      config.ipc = (value == "true");
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
  for (const auto &preloadPath : config.preload) {
    file << "preload = " << preloadPath << "\n";
  }
  for (const auto &wp : config.wallpaper) {
    file << "wallpaper = " << wp.monitorName << ", ";
    switch (wp.mode) {
    case WallpaperMode::COVER:
      file << "";
      break;
    case WallpaperMode::CONTAIN:
      file << "contain:";
      break;
    case WallpaperMode::TILE:
      file << "tile:";
      break;
    case WallpaperMode::FILL:
      file << "fill:";
      break;
    }
    file << wp.imagePath << "\n";
  }
  file << "splash = " << (config.splash ? "true" : "false") << "\n";
  file << "splash_offset = " << config.splash_offset << "\n";
  file << "splash_color = " << config.splash_color << "\n";
  file << "ipc = " << (config.ipc ? "true" : "false") << "\n";
  file.close();
}

HyprpaperConfig HyprpaperParser::getConfig() { return config; }

void HyprpaperParser::setConfig(const HyprpaperConfig &newConfig) {
  config = newConfig;
}
