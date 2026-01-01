#pragma once

#include "configuration.hpp"
#include <memory>
#include <string>
#include <vector>

struct HyprpaperConfig {
  std::vector<WallpaperData> wallpapers;
  bool splash;
  float splash_offset;
  float splash_opacity;
  bool ipc;
};

class HyprpaperParser {
private:
  std::shared_ptr<Configuration> configuration;
  std::string hyprpaperConfigPath;
  HyprpaperConfig hyprpaperConfig;

  bool isValidPath(std::string_view path);
  void printHyprpaperConfig();

public:
  // first wallpaper in hyprpaper.conf
  std::string activeWallpaper;

  HyprpaperParser(std::shared_ptr<Configuration> configuration);

  void parseFile();
  void writeConfigToFile();
  void runHyprCommand(std::string display, std::string wallpaperPath,
                      FitMode mode);
};

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

class HyprpaperLexer {
private:
  std::vector<Token> tokens;

  void printTokens();

public:
  HyprpaperLexer(std::string_view configText);
  const std::vector<Token> &getTokens() const;
};
