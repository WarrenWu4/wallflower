// custom configuration parser
#pragma once

#include <filesystem>
#include <functional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

enum class FitMode { COVER, CONTAIN, TILE, FILL };
const std::unordered_map<FitMode, std::string> fitModeToString({
    {FitMode::COVER, "cover"},
    {FitMode::CONTAIN, "contain"},
    {FitMode::TILE, "tile"},
    {FitMode::FILL, "fill"},
});
const std::unordered_map<std::string, FitMode> stringToFitMode({
    {"cover", FitMode::COVER},
    {"contain", FitMode::CONTAIN},
    {"tile", FitMode::TILE},
    {"fill", FitMode::FILL},
});
const std::vector<std::string> modeToStringUpper({"COVER", "CONTAIN", "TILE",
                                                  "FILL"});

struct WallpaperData {
  std::string path;
  FitMode fitMode;
  std::vector<std::string> monitors;
};

struct WallflowerConfig {
  std::vector<WallpaperData> wallpapers;
  bool splash;
  int splash_offset;
  float splash_opacity;
  bool ipc;
  std::unordered_map<std::string, WallpaperData> preferences;
  std::unordered_set<std::string> searchPaths;
};

struct MonitorInfo {
  std::string name;
  int x, y;
  int width, height;
  bool primary;
};

class Configuration {
private:
  WallflowerConfig config;
  std::vector<MonitorInfo> monitors;
  std::filesystem::path hyprpaperConfigPath;
  std::filesystem::path saveFilePath;

  WallflowerConfig getDefaultConfig();
  std::vector<MonitorInfo> getActiveMonitors();
  void printWallflowerConfig();
  void printMonitorInformation();
  void updateConfigWallpapers(WallpaperData wd);

public:
  std::vector<std::function<void()>> callbackAddPreference;
  std::vector<std::function<void()>> callbackRemovePreference;
  std::vector<std::function<void()>> callbackAddDirectory;
  std::vector<std::function<void()>> callbackRemoveDirectory;

  Configuration();
  ~Configuration();

  const std::vector<MonitorInfo>& getMonitors();

  /*
   * update the user's current wallpaper by
   * running hyprctl ipc under the hood
   * updates app config and hyprpaper.conf
   * with updated data
   */
  void updateWallpaper(std::vector<std::string> display, std::string path, FitMode mode);

  /*
   * parsers hyprpaper.conf and
   * updates app config
   */
  void readHyprpaperConf();

  /*
   * writes the current app
   * config to hyprpaper.conf
   */
  void writeHyprpaperConf();

  /*
   * parses wallflower.save
   * and updates app config
   */
  void readWallflowerSave();

  /*
   * writes the current app
   * config to wallflower.save
   */
  void writeWallflowerSave();

  /*
   * returns a read-only
   * version of app config
   */
  const WallflowerConfig &getConfig();

  /*
   * adds preferences to config
   * checks if the path exists
   */
  void addPreferences(std::vector<WallpaperData> wds, bool overwrite = false);

  /*
   * removes preferences from config 
   */
  void removePreferences(std::vector<WallpaperData> wds);

  /*
   * adds all new directories to the 
   * app config directories parameter
   * validates that all directory paths exists and no duplicates
   */
  void addDirectories(std::vector<std::string> _directories);

  /*
   * removes all directories passed in
   * updates app config directories parameter
   */
  void removeDirectories(std::vector<std::string> _directories);
};

class HyprpaperConfParser {
private:
  enum class TokenType {
    LBRACE,
    RBRACE,
    EQUALS,
    NEWLINE,
    KEYWORD,
    STRING,
  };

  struct Token {
    TokenType tokenType;
    std::string value;
  };

  std::vector<Token> lexer(std::string_view text);
  void parse(const std::vector<Token> &tokens, WallflowerConfig &config);

public:
  HyprpaperConfParser(std::string_view path, WallflowerConfig &config);
};
