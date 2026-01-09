#include "utils.hpp"
#include "logger.hpp"
#include <algorithm>
#include <filesystem>
#include <fstream>
#include <sstream>

namespace Utils {
std::vector<std::string> split(const std::string &s, char d) {
  std::vector<std::string> tokens;
  std::stringstream ss(s);
  std::string token;
  while (std::getline(ss, token, d)) {
    tokens.push_back(token);
  }
  return tokens;
}

std::string removeWhitespace(const std::string &s) {
  std::string result;
  for (char c : s) {
    if (!isspace(c)) {
      result += c;
    }
  }
  return result;
}

const std::filesystem::path getResourcePath() {
#ifdef BUILD_MODE_PROD
  Logger::logMsg(LogLabel::DEBUG,
                 "Using production resource path: \"/usr/share/wallflower/\" ");
  const std::filesystem::path p("/usr/share/wallflower/");
  return p;
#else
  Logger::logMsg(LogLabel::DEBUG,
                 "Using develpment resource path: \"./resources/\"");
  const std::filesystem::path p("./resources/");
  return p;
#endif
}

const std::filesystem::path getSaveFilePath() {
#ifdef BUILD_MODE_PROD
  Logger::logMsg(LogLabel::DEBUG,
                 "Using production save file path: "
                 "\"~/.local/state/wallflower/wallflower.save\" ");
  std::string homeEnv = getenv("HOME");
  const std::filesystem::path p(homeEnv +
                                "/.local/state/wallflower/wallflower.save");
  if (!std::filesystem::exists(p)) {
    try {
      std::filesystem::create_directories(homeEnv + "/.local/state/wallflower");
      std::ofstream f(p.string());
      f.close();
      Logger::logMsg(
          LogLabel::DEBUG,
          "Save file not found. Created one in \"~/.local/state/wallflower/\"");
    } catch (const std::exception &e) {
      Logger::logMsg(LogLabel::ERROR,
                     "Uknown error occurred: " + std::string(e.what()));
    }
  }
  return p;
#else
  Logger::logMsg(
      LogLabel::DEBUG,
      "Using development save file path: \"./resources/wallflower.save\"");
  const std::filesystem::path p("./resources/wallflower.save");
  if (!std::filesystem::exists(p)) {
    try {
      std::filesystem::create_directories("./resources");
      std::ofstream f(p.string());
      f.close();
      Logger::logMsg(LogLabel::DEBUG,
                     "Save file not found. Created one in \"./resources\"");
    } catch (const std::exception &e) {
      Logger::logMsg(LogLabel::ERROR,
                     "Uknown error occurred: " + std::string(e.what()));
    }
  }
  return p;
#endif
}

bool isValidHyprPath(std::string_view imagePath) {
  if (imagePath.size() <= 0) {
    return false;
  }
  if (imagePath.at(0) != '~' && imagePath.at(0) != '/') {
    return false;
  }
  std::string absPath = std::string(imagePath);
  if (imagePath.at(0) == '~') {
    absPath = std::string(std::getenv("HOME")) +
              std::string(imagePath.substr(1, imagePath.size()));
  }
  std::filesystem::path p(absPath);
  return std::filesystem::exists(p);
}

std::string toUpperString(std::string str) {
  std::string new_str = "";
  for (const char &c : str) {
    new_str += std::toupper(c);
  }
  return new_str;
}

std::vector<std::string> getImagesInDirectory(std::string path) {
  std::vector<std::string> res;
  std::filesystem::path p(path);
  if (std::filesystem::exists(p) && std::filesystem::is_directory(p)) {
    for (const auto &entry : std::filesystem::directory_iterator(p)) {
      if (entry.is_regular_file()) {
        std::string ext = entry.path().extension().string();
        std::transform(ext.begin(), ext.end(), ext.begin(), ::tolower);
        if (ext == ".png" || ext == ".jpg" || ext == ".jpeg") {
          res.push_back(entry.path());
        }
      }
    }
  }
  return res;
}
} // namespace Utils
