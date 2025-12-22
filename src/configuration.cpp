#include "configuration.hpp"
#include "logger.hpp"
#include "raylib.h"
#include <algorithm>
#include <filesystem>
#include <fstream>

Configuration::Configuration() {
  configurationPath = "/home/warrenwu/projects/wallflower/resources/configuration.txt";

  // create configuration file & path if it does not exist
  std::filesystem::path p(configurationPath);
  if (!std::filesystem::exists(p)) {
    try {
      // INFO: configuratin.txt will always be file name and is always 17 characters +1 (for the extra /)
      Logger::logMsg(LogLabel::DEBUG, "Configuration path does not exist. Creating path and file.");
      std::filesystem::create_directories(configurationPath.substr(0, configurationPath.size()-18));
      std::ofstream file(configurationPath);
      file.close();
      Logger::logMsg(LogLabel::OK, "Configuration file created.");
    } catch (const std::exception& e) {
      Logger::logMsg(LogLabel::FAIL, "Unknown error: " + std::string(e.what()));
    }
  }

  parseConfiguration();
  scanDirectories();

  // FIX: current naive loading could have problems with large galleries
  // in the future implement lazy loading and remove this naive load all images logic
  std::vector<std::string> paths(wallpapers.size());
  int i = 0;
  for (auto it = wallpapers.begin(); it != wallpapers.end(); it++, i++) {
    paths[i] = (*it).first;
  }
  loadWallpapers(paths);
}

std::vector<std::string> Configuration::getImagesFromDirectories(std::vector<std::string> paths) {
  std::vector<std::string> res;
  for (const std::string& directory : paths) {
    std::filesystem::path p(directory);
    if (std::filesystem::exists(p) && std::filesystem::is_directory(p)) {
      for (const auto& entry : std::filesystem::directory_iterator(p)) {
        if (entry.is_regular_file()) {
          std::string ext = entry.path().extension().string();
          std::transform(ext.begin(), ext.end(), ext.begin(), ::tolower);
          if (ext == ".png" || ext == ".jpg" || ext == ".jpeg") {
            res.push_back(entry.path());
          }
        }
      }
    } else {
      Logger::logMsg(LogLabel::FAIL, "Path is not a directory or does not exist.");
    }
  }
  return res;
}

void Configuration::addDirectory(std::string path) {
  // add found images to wallpaper only if it is a new image
  // load image texture via loadWallpapers()
  // update directories set
  // INFO: no need to check if path exists or if it is a directory since getImagesFromDirectories() already handles it and returns an empty vector if not valid
  std::vector<std::string> images = getImagesFromDirectories({path});
  for (const std::string& image : images) {
    if (wallpapers.find(image) == wallpapers.end()) {
      wallpapers[image] = (WallpaperData) {
        .path = image,
        .fitMode = FitMode::COVER,
        .monitor = ""
      };
    }
  }
  loadWallpapers(images);
  directories.insert(path);
}

void Configuration::removeDirectory(std::string path) {
  // remove images from wallpapers and wallpaperImages 
  // update directories set
  std::vector<std::string> images = getImagesFromDirectories({path});
  for (const std::string& image : images) {
    wallpapers.erase(image);
  }
  unloadWallpapers(images);
  directories.erase(path);
}

Configuration::~Configuration() {
  updateConfiguration();
  for (auto it = wallpaperImages.begin(); it != wallpaperImages.end(); it++) {
    UnloadTexture(it->second.image);
  }
}

void Configuration::parseConfiguration() {
  std::ifstream file(this->configurationPath);
  if (!file.is_open()) {
    Logger::logMsg(LogLabel::ERROR, "Unable to open configuration file " + this->configurationPath);
    return;
  }
  std::string line;
  bool reachedSeparator = false;
  while (std::getline(file, line)) {
    if (line == "") {
      reachedSeparator = true;
      continue;
    }
    if (!reachedSeparator) {
      this->directories.insert(line);
    } else {
      std::stringstream ss(line);
      std::string key, value;

      if (std::getline(ss, key, ',') && std::getline(ss, value)) {
        // TODO: add parsing support for monitors
        // just use empty string as default for now
        std::filesystem::path p(value);
        if (!std::filesystem::exists(p)) { continue; }
        wallpapers[key] = (WallpaperData) {
          .path = key,
          .fitMode = stringToFitMode.at(value),
          .monitor = ""
        };
      }
    }
  }
  file.close();
  Logger::logMsg(LogLabel::OK, "Configuration parsed");
}

void Configuration::scanDirectories() {
  for (auto it = directories.begin(); it != directories.end(); it++) {
    const std::string& directory = *it;
    std::filesystem::path p(directory);
    if (std::filesystem::exists(p) && std::filesystem::is_directory(p)) {
      for (const auto& entry : std::filesystem::directory_iterator(p)) {
        if (entry.is_regular_file()) {
          std::string ext = entry.path().extension().string();
          std::transform(ext.begin(), ext.end(), ext.begin(), ::tolower);
          if ((ext == ".png" || ext == ".jpg" || ext == ".jpeg") && wallpapers.find(entry.path()) == wallpapers.end()) {
            wallpapers[entry.path()] = (WallpaperData) {
              .path = entry.path(),
              .fitMode = FitMode::COVER,
              .monitor = ""
            };
          }
        }
      }
    } else {
      Logger::logMsg(LogLabel::FAIL, "Path is not a directory or does not exist. Removing the path from configuration.");
      directories.erase(directory);
    }
  }
  Logger::logMsg(LogLabel::OK, "Directories scanned");
}

void Configuration::loadWallpapers(std::vector<std::string> paths) {
  for (const std::string& path : paths) {
    std::filesystem::path p(path);
    if (!std::filesystem::exists(p)) {
      Logger::logMsg(LogLabel::FAIL, "Wallpaper path \"" + path + "\" does not exist");
      continue;
    }
    Texture2D image = LoadTexture(path.c_str());
    float aspectRatio = static_cast<float>(image.width) / image.height;
    wallpaperImages[path] = (WallpaperImage) {
      .image = image,
      .aspectRatio = aspectRatio
    };
  }
}

void Configuration::unloadWallpapers(std::vector<std::string> paths) {
  for (const std::string& path : paths) {
    if (wallpaperImages.find(path) == wallpaperImages.end()) {
      Logger::logMsg(LogLabel::FAIL, "Unable to unload \"" + path + "\" as it is not found");
      continue;
    }
    UnloadTexture(wallpaperImages.at(path).image);
    wallpaperImages.erase(path);
    Logger::logMsg(LogLabel::OK, "Unloaded \"" + path + "\" from memory");
  }
}

void Configuration::updateConfiguration() {
  std::ofstream file(this->configurationPath);
  if (!file.is_open()) {
    Logger::logMsg(LogLabel::ERROR, "Unable to open configuration file " + this->configurationPath);
    return;
  }
  for (auto it = directories.begin(); it != directories.end(); it++) {
    std::string directory = *it;
    file << directory << "\n";
  }
  file << "\n";
  // TODO: add format for adding monitors
  for (auto it = wallpapers.begin(); it != wallpapers.end(); it++) {
    std::string path = (*it).second.path;
    std::string fitMode = fitModeToString.at((*it).second.fitMode);
    file << path << "," << fitMode << "\n";
  }
  file.close();
  Logger::logMsg(LogLabel::OK, "Configuration updated");
}
