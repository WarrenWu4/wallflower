// hypr utility helper functions and classes

#include "hyprmanager.hpp"
#include "configuration.hpp"
#include "logger.hpp"
#include <filesystem>
#include <fstream>
#include <iostream>

/*
 * Checks path is a valid hyprpaper wallpaper path
 *
 * In order to be valid the following has to be true:
 * 1. file has to exist on the system
 * 2. path must start with '~' or '/'
 */
bool HyprpaperParser::isValidPath(std::string_view path) {
  if (path.size() <= 0) {
    return false;
  }
  if (path.at(0) != '~' && path.at(0) != '/') {
    return false;
  }
  std::string absPath = std::string(path);
  if (path.at(0) == '~') {
    absPath = std::string(std::getenv("HOME")) + std::string(path.substr(1, path.size()));
  }
  std::filesystem::path p(absPath);
  return std::filesystem::exists(p);
}

void HyprpaperParser::printHyprpaperConfig() {
  std::string debugMsg = "Hyprpaper Config:\n";
  for (size_t i = 0; i < hyprpaperConfig.wallpapers.size(); i++) {
    WallpaperData wd = hyprpaperConfig.wallpapers.at(i);
    debugMsg += "Wallpaper: " + wd.monitor + "," + wd.path + "," +
                fitModeToString.at(wd.fitMode) + "\n";
  }
  debugMsg += "Splash: " + std::to_string(hyprpaperConfig.splash) + "\n";
  debugMsg +=
      "Splash Offset: " + std::to_string(hyprpaperConfig.splash_offset) + "\n";
  debugMsg +=
      "Splash Opacity: " + std::to_string(hyprpaperConfig.splash_opacity) +
      "\n";
  debugMsg += "IPC: " + std::to_string(hyprpaperConfig.ipc) + "\n";
  Logger::logMsg(LogLabel::DEBUG, debugMsg);
}

HyprpaperParser::HyprpaperParser(std::shared_ptr<Configuration> configuration) {
  this->configuration = configuration;
  hyprpaperConfigPath =
      std::string(getenv("HOME")) + "/.config/hypr/hyprpaper.conf";
  hyprpaperConfig = (HyprpaperConfig){.wallpapers = {},
                                      .splash = false,
                                      .splash_offset = 20,
                                      .splash_opacity = 0.8,
                                      .ipc = true};
  activeWallpaper = "";
  parseFile();
  printHyprpaperConfig();
}

void HyprpaperParser::runHyprCommand(std::string display,
                                     std::string wallpaperPath, FitMode mode) {
  display += ",";
  wallpaperPath += ",";
  std::string wallpaperCmd = "hyprctl hyprpaper wallpaper \"" + display +
                             wallpaperPath + fitModeToString.at(mode) + "\"";
  Logger::logMsg(LogLabel::DEBUG, "Running wallpaper command: " + wallpaperCmd);
  std::system(wallpaperCmd.c_str());
  this->activeWallpaper = wallpaperPath;
  this->writeConfigToFile();
}

void HyprpaperParser::parseFile() {
  std::ifstream file(hyprpaperConfigPath);
  if (!file.is_open()) {
    throw std::runtime_error("Could not open file: " + hyprpaperConfigPath);
  }
  std::string data((std::istreambuf_iterator<char>(file)),
                   std::istreambuf_iterator<char>());
  file.close();

  // run lexer and get tokens
  // loop through tokens to build configuration
  HyprpaperLexer hl = HyprpaperLexer(data);
  const std::vector<Token> tokens = hl.getTokens();
  size_t pos = 0;
  while (pos < tokens.size()) {
    // create wallpaper data and add to configuration
    if (tokens.at(pos).tokenType == TokenType::LBRACE) {
      WallpaperData wd = (WallpaperData){
          .path = "",
          .fitMode = FitMode::COVER,
          .monitor = "",
      };
      while (tokens.at(pos).tokenType != TokenType::RBRACE) {
        if (tokens.at(pos).tokenType == TokenType::KEYWORD) {
          if (tokens.at(pos).value == "path") {
            if (!isValidPath(tokens.at(pos+2).value)) {
              throw std::runtime_error("Invalid path found in hyprpaper.conf");
            }
            wd.path = tokens.at(pos + 2).value;
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
      hyprpaperConfig.wallpapers.push_back(wd);
    }

    // handle all other assignments
    if (tokens.at(pos).tokenType == TokenType::KEYWORD) {
      if (tokens.at(pos).value == "splash") {
        hyprpaperConfig.splash = (tokens.at(pos + 2).value == "true");
      } else if (tokens.at(pos).value == "splash_offset") {
        hyprpaperConfig.splash_offset = std::stoi(tokens.at(pos + 2).value);
      } else if (tokens.at(pos).value == "splash_opacity") {
        hyprpaperConfig.splash_opacity = std::stof(tokens.at(pos + 2).value);
      } else if (tokens.at(pos).value == "ipc") {
        hyprpaperConfig.ipc = (tokens.at(pos + 2).value == "true");
      } else {
        throw std::runtime_error("Error parsing hyprpaper.conf: Uknown field " +
                                 tokens.at(pos).value);
      }
    }
    pos++;
  }
}

void HyprpaperParser::writeConfigToFile() {
  // create .bak file for old config
  std::ofstream backupFile(hyprpaperConfigPath + ".bak");
  std::ifstream originalFile(hyprpaperConfigPath);
  if (!originalFile.is_open()) {
    throw std::runtime_error("Could not open file: " + hyprpaperConfigPath);
  }
  backupFile << originalFile.rdbuf();
  backupFile.close();
  originalFile.close();

  // update existing config file
  std::ofstream file(hyprpaperConfigPath);
  for (size_t i = 0; i < hyprpaperConfig.wallpapers.size(); i++) {
    WallpaperData wd = hyprpaperConfig.wallpapers.at(i);
    file << "wallpaper {\n";
    file << "\tmonitor = " << wd.monitor << "\n";
    file << "\tpath = " << wd.path << "\n";
    file << "\tfit_mode = " << fitModeToString.at(wd.fitMode) << "\n";
    file << "{\n";
  }
  file << "splash = " << (hyprpaperConfig.splash ? "true" : "false") << "\n";
  file << "splash_offset = " << hyprpaperConfig.splash_offset << "\n";
  file << "splash_opacity = " << hyprpaperConfig.splash_opacity << "\n";
  file << "ipc = " << (hyprpaperConfig.ipc ? "true" : "false") << "\n";
  file.close();
}

void HyprpaperLexer::printTokens() {
  std::string debugMsg = "Printing Lexer Tokens:\n";
  for (size_t i = 0; i < tokens.size(); i++) {
    debugMsg += std::to_string(static_cast<int>(tokens.at(i).tokenType)) + " ";
    if (tokens.at(i).value == "\n") {
      debugMsg += "_";
    } else {
      debugMsg += tokens.at(i).value;
    }
    debugMsg += ", ";
  }
  Logger::logMsg(LogLabel::DEBUG, debugMsg);
}

HyprpaperLexer::HyprpaperLexer(std::string_view configText) {
  bool isComment = false;
  bool isKey = true;
  std::string value = "";
  for (size_t i = 0; i < configText.size(); i++) {
    char c = configText.at(i);

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
  // printTokens();
}

const std::vector<Token> &HyprpaperLexer::getTokens() const { return tokens; }
