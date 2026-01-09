#include "configuration.hpp"
#include "logger.hpp"
#include "utils.hpp"
#include <cassert>
#include <filesystem>
#include <fstream>

WallflowerConfig Configuration::getDefaultConfig() {
  return (WallflowerConfig){
      .wallpapers = {},
      .splash = true,
      .splash_offset = 20,
      .splash_opacity = 0.8,
      .ipc = true,
      .preferences = {},
      .directories = {},
  };
}

void Configuration::printWallflowerConfig() {
  std::string msg = "";
  for (size_t i = 0; i < config.wallpapers.size(); i++) {
    WallpaperData wd = config.wallpapers.at(i);
    msg += "wallpaper = " + wd.monitor + "," + wd.path + "," +
           fitModeToString.at(wd.fitMode) + "\n";
  }
  msg += "splash = " + std::string(config.splash ? "true" : "false") + "\n";
  msg += "splash_offset = " + std::to_string(config.splash_offset) + "\n";
  msg += "splash_opacity= " + std::to_string(config.splash_opacity) + "\n";
  msg += "ipc = " + std::string(config.splash ? "true" : "false") + "\n\n";
  for (auto it = config.preferences.begin(); it != config.preferences.end();
       it++) {
    WallpaperData wd = (*it).second;
    msg += "wallpaper_preference = " + wd.monitor + "," + wd.path + "," +
           fitModeToString.at(wd.fitMode) + "\n";
  }
  for (auto it = config.directories.begin(); it != config.directories.end();
       it++) {
    msg += "directory = " + *it;
  }
  Logger::logMsg(LogLabel::DEBUG, "Wallflower Config\n" + msg);
}

Configuration::Configuration() {
  config = getDefaultConfig();
  hyprpaperConfigPath = std::filesystem::path(std::string(getenv("HOME")) +
                                              "/.config/hypr/hyprpaper.conf");
  saveFilePath = Utils::getSaveFilePath();

  if (std::filesystem::exists(hyprpaperConfigPath)) {
    readHyprpaperConf();
    Logger::logMsg(LogLabel::OK, "Parsing hyprpaper.conf successful.");
  } else {
    Logger::logMsg(
        LogLabel::DEBUG,
        "No hyprpaper.conf file found. Skipping hyprpaper.conf parsing step.");
  }

  if (std::filesystem::exists(saveFilePath)) {
    readWallflowerSave();
    Logger::logMsg(LogLabel::OK, "Parsing wallflower.save successful.");
  } else {
    Logger::logMsg(LogLabel::DEBUG, "No wallflower.save file found. Skipping "
                                    "wallflower.save parsing step.");
  }

  printWallflowerConfig();
}

Configuration::~Configuration() {
  writeHyprpaperConf();
  writeWallflowerSave();
}

void Configuration::updateWallpaper(std::string display, std::string path,
                                    FitMode mode) {
  WallpaperData wd =
      (WallpaperData){.path = path, .fitMode = mode, .monitor = display};
  display += ",";
  path += ",";
  std::string wallpaperCmd = "hyprctl hyprpaper wallpaper \"" + display + path +
                             fitModeToString.at(mode) + "\"";
  Logger::logMsg(LogLabel::DEBUG, "Running wallpaper command: " + wallpaperCmd);
  std::system(wallpaperCmd.c_str());
  // TODO: update to support multiple monitors in the future
  this->config.wallpapers[0] = wd;
  this->writeHyprpaperConf();
}

void Configuration::readHyprpaperConf() {
  HyprpaperConfParser(hyprpaperConfigPath.string(), config);
}

void Configuration::writeHyprpaperConf() {
  // create .bak file for old config
  std::ofstream backupFile(hyprpaperConfigPath.string() + ".bak");
  std::ifstream originalFile(hyprpaperConfigPath);
  if (!originalFile.is_open()) {
    throw std::runtime_error("Could not open file: " +
                             hyprpaperConfigPath.string());
  }
  backupFile << originalFile.rdbuf();
  backupFile.close();
  originalFile.close();

  // update existing config file
  std::ofstream file(hyprpaperConfigPath);
  for (size_t i = 0; i < config.wallpapers.size(); i++) {
    WallpaperData wd = config.wallpapers.at(i);
    file << "wallpaper {\n";
    file << "\tmonitor = " << wd.monitor << "\n";
    file << "\tpath = " << wd.path << "\n";
    file << "\tfit_mode = " << fitModeToString.at(wd.fitMode) << "\n";
    file << "}\n";
  }
  file << "splash = " << (config.splash ? "true" : "false") << "\n";
  file << "splash_offset = " << config.splash_offset << "\n";
  file << "splash_opacity = " << config.splash_opacity << "\n";
  file << "ipc = " << (config.ipc ? "true" : "false") << "\n";
  file.close();
}

void Configuration::readWallflowerSave() {
  std::ifstream f(this->saveFilePath);
  assert(f.is_open() &&
         "readWallflowerSave() expects wallflower.save file to exist.");
  std::string line;
  bool reachedSeparator = false;
  while (std::getline(f, line)) {
    if (line == "") {
      reachedSeparator = true;
      continue;
    }
    if (!reachedSeparator) {
      config.directories.insert(line);
    } else {
      std::stringstream ss(line);
      std::string key, value;

      if (std::getline(ss, key, ',') && std::getline(ss, value)) {
        // TODO: add parsing support for monitors
        // just use empty string as default for now
        std::filesystem::path p(key);
        if (!std::filesystem::exists(p)) {
          continue;
        }
        config.preferences[key] = (WallpaperData){
            .path = key, .fitMode = stringToFitMode.at(value), .monitor = ""};
      }
    }
  }
  f.close();
}

void Configuration::writeWallflowerSave() {
  std::ofstream f(saveFilePath);
  assert(f.is_open() &&
         "writeWallflowerSave(): unable to open configuration file");
  for (auto it = config.directories.begin(); it != config.directories.end();
       it++) {
    f << *it << "\n";
  }
  f << "\n";
  // TODO: add format for adding monitors
  for (auto it = config.preferences.begin(); it != config.preferences.end(); it++) {
    WallpaperData wd = (*it).second;
    f << wd.path << "," << fitModeToString.at(wd.fitMode) << "\n";
  }
  f.close();
}

const WallflowerConfig &Configuration::getConfig() { return config; }

void Configuration::addPreferences(std::vector<WallpaperData> wds) {
  for (const WallpaperData& wd: wds) {
    std::filesystem::path p(wd.path);
    if (std::filesystem::exists(p)) {
      config.preferences[wd.path] = wd;
    }
  }
  for (auto& callback : callbackAddPreference) {
    callback();
  }
}

void Configuration::removePreferences(std::vector<WallpaperData> wds) {
  for (const WallpaperData& wd : wds) {
    config.preferences.erase(wd.path);
  }
  for (auto &callback : callbackRemovePreference) {
    callback();
  }
}

void Configuration::addDirectories(std::vector<std::string> _directories) {
  for (const std::string &dir : _directories) {
    std::filesystem::path p(dir);
    if (config.directories.find(dir) == config.directories.end() &&
        std::filesystem::exists(dir)) {
      config.directories.insert(dir);
    }
  }
  for (auto &callback : callbackAddDirectory) {
    callback();
  }
}

void Configuration::removeDirectories(std::vector<std::string> _directories) {
  for (const std::string &dir : _directories) {
    config.directories.erase(dir);
  }
  for (auto &callback : callbackRemoveDirectory) {
    callback();
  }
}

HyprpaperConfParser::HyprpaperConfParser(std::string_view path,
                                         WallflowerConfig &config) {
  std::ifstream f(path.data());
  if (!f.is_open()) {
    throw std::runtime_error("Could not open file: " + std::string(path));
  }
  std::string text((std::istreambuf_iterator<char>(f)),
                   std::istreambuf_iterator<char>());
  f.close();
  std::vector<Token> tokens = lexer(text);
  parse(tokens, config);
}

std::vector<HyprpaperConfParser::Token>
HyprpaperConfParser::lexer(std::string_view text) {
  std::vector<Token> tokens;
  bool isComment = false;
  bool isKey = true;
  std::string value = "";
  for (size_t i = 0; i < text.size(); i++) {
    char c = text.at(i);
    isComment = (c == '#' || (isComment && c != '\n'));
    if (c == ' ' || c == '\t' || isComment) {
      continue;
    }
    if (c == '{') {
      tokens.push_back((Token){TokenType::LBRACE, "{"});
      value = "";
    } else if (c == '}') {
      tokens.push_back((Token){TokenType::RBRACE, "}"});
      value = "";
    } else if (c == '=') {
      if (value != "") {
        TokenType t = (isKey) ? TokenType::KEYWORD : TokenType::STRING;
        tokens.push_back((Token){t, value});
      }
      tokens.push_back((Token){TokenType::EQUALS, "="});
      isKey = false;
      value = "";
    } else if (c == '\n') {
      if (value != "") {
        TokenType t = (isKey) ? TokenType::KEYWORD : TokenType::STRING;
        tokens.push_back((Token){t, value});
      }
      tokens.push_back((Token){TokenType::NEWLINE, "\n"});
      isKey = true;
      value = "";
    } else {
      value += c;
    }
  }
  return tokens;
}

void HyprpaperConfParser::parse(const std::vector<Token> &tokens,
                                WallflowerConfig &config) {
  size_t pos = 0;
  while (pos < tokens.size()) {
    // handle wallpaper object
    if (tokens.at(pos).tokenType == TokenType::LBRACE) {
      WallpaperData wd = (WallpaperData){
          .path = "",
          .fitMode = FitMode::COVER,
          .monitor = "",
      };
      while (tokens.at(pos).tokenType != TokenType::RBRACE) {
        if (tokens.at(pos).tokenType == TokenType::KEYWORD) {
          if (tokens.at(pos).value == "path") {
            if (!Utils::isValidHyprPath(tokens.at(pos + 2).value)) {
              throw std::runtime_error("Invalid path found in hyprpaper.conf");
            }
            // INFO: convert everything to absolute path
            if (tokens.at(pos + 2).value.at(0) == '~') {
              wd.path = std::string(getenv("HOME")) + tokens.at(pos + 2).value.substr(1);
            } else {
              wd.path = tokens.at(pos + 2).value;
            }
          } else if (tokens.at(pos).value == "monitor") {
            if (tokens.at(pos + 2).value == "\n") {
              wd.monitor = "";
            } else {
              wd.monitor = tokens.at(pos + 2).value;
            }
          } else if (tokens.at(pos).value == "fit_mode") {
            if (tokens.at(pos + 2).value == "\n") {
              wd.fitMode = FitMode::COVER;
            } else {
              wd.fitMode = stringToFitMode.at(tokens.at(pos + 2).value);
            }
          } else {
            throw std::runtime_error(
                "Error parsing hyprpaper.conf: Uknown field for wallpaper " +
                tokens.at(pos).value);
          }
        }
        pos++;
      }
      config.wallpapers.push_back(wd);
    }

    // handle all other assignments
    if (tokens.at(pos).tokenType == TokenType::KEYWORD) {
      if (tokens.at(pos).value == "splash") {
        config.splash = (tokens.at(pos + 2).value == "true");
      } else if (tokens.at(pos).value == "splash_offset") {
        config.splash_offset = std::stoi(tokens.at(pos + 2).value);
      } else if (tokens.at(pos).value == "splash_opacity") {
        config.splash_opacity = std::stof(tokens.at(pos + 2).value);
      } else if (tokens.at(pos).value == "ipc") {
        config.ipc = (tokens.at(pos + 2).value == "true");
      } else {
        throw std::runtime_error("Error parsing hyprpaper.conf: Uknown field " +
                                 tokens.at(pos).value);
      }
    }
    pos++;
  }
}
