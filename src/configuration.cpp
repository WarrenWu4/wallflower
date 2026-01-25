#include "configuration.hpp"
#include "logger.hpp"
#include "utils.hpp"
#include <cassert>
#include <filesystem>
#include <fstream>
#include <X11/Xlib.h>
#include <X11/extensions/Xrandr.h>
#include <wayland-client.h>

WallflowerConfig Configuration::getDefaultConfig() {
  return (WallflowerConfig){
      .wallpapers = {},
      .splash = true,
      .splash_offset = 20,
      .splash_opacity = 0.8,
      .ipc = true,
      .preferences = {},
      .searchPaths = {},
  };
}

std::vector<MonitorInfo> Configuration::getActiveMonitors() {
  std::vector<MonitorInfo> monitors = {};
  
  // try to open display
  Display* display = XOpenDisplay(nullptr);
  if (!display) {
    Logger::logMsg(LogLabel::FAIL, "Could not open display");
    return monitors;
  }
  
  Window root = DefaultRootWindow(display);
  
  // get screen resources using XRandR
  XRRScreenResources* resources = XRRGetScreenResources(display, root);
  if (!resources) {
    Logger::logMsg(LogLabel::FAIL, "Could not get resources using XRandR");
    XCloseDisplay(display);
    return monitors;
  }
  
  // iterate through all outputs
  for (int i = 0; i < resources->noutput; i++) {
    XRROutputInfo* outputInfo = XRRGetOutputInfo(display, resources, resources->outputs[i]);
    
    if (outputInfo->connection == RR_Connected && outputInfo->crtc) {
      // get monitor information through XRRGetCrtcInfo
      XRRCrtcInfo* crtcInfo = XRRGetCrtcInfo(display, resources, outputInfo->crtc); 
      MonitorInfo monitor;
      monitor.name = outputInfo->name;
      monitor.x = crtcInfo->x;
      monitor.y = crtcInfo->y;
      monitor.width = crtcInfo->width;
      monitor.height = crtcInfo->height;
      
      // Check if this is the primary monitor
      RROutput primary = XRRGetOutputPrimary(display, root);
      monitor.primary = (resources->outputs[i] == primary);
      
      monitors.push_back(monitor);
      XRRFreeCrtcInfo(crtcInfo);
    }
    XRRFreeOutputInfo(outputInfo);
  }
  XRRFreeScreenResources(resources);
  XCloseDisplay(display);
  return monitors;
}

void Configuration::printWallflowerConfig() {
  std::string msg = "";
  for (size_t i = 0; i < config.wallpapers.size(); i++) {
    WallpaperData wd = config.wallpapers.at(i);
    for (size_t j = 0; j < wd.monitors.size(); j++) {
      msg += "wallpaper = " + wd.monitors.at(j) + "," + wd.path + "," +
             fitModeToString.at(wd.fitMode) + "\n";

    }
  }
  msg += "splash = " + std::string(config.splash ? "true" : "false") + "\n";
  msg += "splash_offset = " + std::to_string(config.splash_offset) + "\n";
  msg += "splash_opacity= " + std::to_string(config.splash_opacity) + "\n";
  msg += "ipc = " + std::string(config.splash ? "true" : "false") + "\n\n";
  for (auto it = config.preferences.begin(); it != config.preferences.end();
       it++) {
    WallpaperData wd = (*it).second;
    for (size_t i = 0; i < wd.monitors.size(); i++) {
      msg += "wallpaper_preference = " + wd.monitors.at(i) + "," + wd.path + "," + fitModeToString.at(wd.fitMode) + "\n";
    }
  }
  for (auto it = config.searchPaths.begin(); it != config.searchPaths.end();
       it++) {
    msg += "searchPath = " + *it;
  }
  Logger::logMsg(LogLabel::DEBUG, "Wallflower Config\n" + msg);
}

void Configuration::printMonitorInformation() {
  for (const MonitorInfo& monitor : monitors) {
    std::string msg = "Monitor found: " + monitor.name + ", {x: " + std::to_string(monitor.x) + ", y: " + std::to_string(monitor.y) + "}, {width: " + std::to_string(monitor.width) + ", height: " + std::to_string(monitor.height) + "}" + ", " + (monitor.primary ? "primary" : "not primary");
    Logger::logMsg(LogLabel::DEBUG, msg);
  }
}

void Configuration::updateConfigWallpapers(WallpaperData wd) {
  // only add to config.wallpapers if path is unique
  for (size_t i = 0; i < config.wallpapers.size(); i++) {
    if (config.wallpapers.at(i).path == wd.path) {
      // path is not unique so combine 
      // monitors vectors and remove dupes
      std::unordered_set<std::string> uniqueMonitors = {}; 
      std::vector<std::string> newMonitors = {};
      for (size_t j = 0; j < config.wallpapers.at(i).monitors.size(); j++) {
        if (uniqueMonitors.contains(config.wallpapers.at(i).monitors.at(j))) {
          continue;
        }
        newMonitors.push_back(config.wallpapers.at(i).monitors.at(j));
        uniqueMonitors.insert(config.wallpapers.at(i).monitors.at(j));
      }
      for (size_t j = 0; j < wd.monitors.size(); j++) {
        if (uniqueMonitors.contains(wd.monitors.at(j))) {
          continue;
        }
        newMonitors.push_back(wd.monitors.at(j));
        uniqueMonitors.insert(wd.monitors.at(j));
      }
      config.wallpapers.at(i).monitors = newMonitors;
      return;
    }
  }

  // path has to be unique
  // so add new entry to wallpapers
  config.wallpapers.push_back(wd);
}

Configuration::Configuration() {
  config = getDefaultConfig();
  monitors = getActiveMonitors();
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
  Logger::logMsg(LogLabel::OK, "Destructor ran");
}

const std::vector<MonitorInfo>& Configuration::getMonitors() {
  return monitors;
} 

void Configuration::updateWallpaper(std::vector<std::string> display, std::string path, FitMode mode) {
  WallpaperData wd = (WallpaperData) {.path = path, .fitMode = mode, .monitors = display};

  // empty vector sets wallpaper to all displays
  if (display.empty()) {
    std::string cmdDisplay = ",";
    std::string cmdPath = path + ",";
    std::string wallpaperCmd = "hyprctl hyprpaper wallpaper \"" + cmdDisplay + cmdPath + fitModeToString.at(mode) + "\"";
    Logger::logMsg(LogLabel::DEBUG, "Running wallpaper command: " + wallpaperCmd);
    std::system(wallpaperCmd.c_str());
  } else {
    for (size_t i = 0; i < display.size(); i++) {
      std::string cmdDisplay = display.at(i) + ",";
      std::string cmdPath = path + ",";
      std::string wallpaperCmd = "hyprctl hyprpaper wallpaper \"" + cmdDisplay + cmdPath + fitModeToString.at(mode) + "\"";
      Logger::logMsg(LogLabel::DEBUG, "Running wallpaper command: " + wallpaperCmd);
      std::system(wallpaperCmd.c_str());
    }
  }
  this->updateConfigWallpapers(wd);
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
    for (size_t j = 0; j < wd.monitors.size(); j++) {
      file << "wallpaper {\n";
      file << "\tmonitor = " << wd.monitors.at(j) << "\n";
      file << "\tpath = " << wd.path << "\n";
      file << "\tfit_mode = " << fitModeToString.at(wd.fitMode) << "\n";
      file << "}\n";
    }
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
      config.searchPaths.insert(line);
    } else {
      std::vector<std::string> values = Utils::split(line, ',');
      if (values.size() < 2 || values.size() > 3) {
        Logger::logMsg(LogLabel::FAIL, "unexpected number of parameters in preference line");
        continue;
      }
      std::filesystem::path p(values.at(0));
      if (!std::filesystem::exists(p)) {
        continue;
      }
      // maintain backwards compat where
      // monitors are not in prev pref line
      // bad design by me previously...
      if (values.size() == 2) {
        config.preferences[values.at(0)] = (WallpaperData) {
          .path = values.at(0),
          .fitMode = stringToFitMode.at(values.at(1)),
          .monitors = {}
        };
      } else if (values.size() == 3) {
        if (config.preferences.contains(values.at(0))) {
          config.preferences.at(values.at(0)).monitors.push_back(values.at(2));
        } else {
          config.preferences[values.at(0)] = (WallpaperData) {
            .path = values.at(0),
            .fitMode = stringToFitMode.at(values.at(1)),
            .monitors = {values.at(2)}
          };
        }
      }

    }
  }
  f.close();
}

void Configuration::writeWallflowerSave() {
  std::ofstream f(saveFilePath);
  assert(f.is_open() &&
         "writeWallflowerSave(): unable to open configuration file");
  for (auto it = config.searchPaths.begin(); it != config.searchPaths.end();
       it++) {
    f << *it << "\n";
  }
  f << "\n";
  for (auto it = config.preferences.begin(); it != config.preferences.end(); it++) {
    WallpaperData wd = (*it).second;
    for (size_t i = 0; i < wd.monitors.size(); i++) {
      f << wd.monitors.at(i) << "," << wd.path << "," << fitModeToString.at(wd.fitMode) << "\n";
    }
  }
  f.close();
}

const WallflowerConfig &Configuration::getConfig() { return config; }

void Configuration::addPreferences(std::vector<WallpaperData> wds, bool overwrite) {
  for (const WallpaperData& wd: wds) {
    std::filesystem::path p(wd.path);
    if (std::filesystem::exists(p)) {
      if ((!overwrite && config.preferences.find(wd.path) == config.preferences.end()) || overwrite) {
        config.preferences[wd.path] = wd;
      }
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
    if (config.searchPaths.find(dir) == config.searchPaths.end() &&
        std::filesystem::exists(dir)) {
      config.searchPaths.insert(dir);
    }
  }
  for (auto &callback : callbackAddDirectory) {
    callback();
  }
}

void Configuration::removeDirectories(std::vector<std::string> _directories) {
  for (const std::string &dir : _directories) {
    config.searchPaths.erase(dir);
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
          .monitors = {}
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
            if (tokens.at(pos + 2).value != "\n") {
              wd.monitors.push_back(tokens.at(pos + 2).value); 
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
