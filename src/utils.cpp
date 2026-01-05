#include "utils.hpp"
#include "logger.hpp"
#include <sstream>
#include <filesystem>
#include <fstream>

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
  Logger::logMsg(LogLabel::DEBUG, "Using production resource path: \"/usr/share/wallflower/\" ");
  const std::filesystem::path p("/usr/share/wallflower/");
  return p;
#else
  Logger::logMsg(LogLabel::DEBUG, "Using develpment resource path: \"./resources/\"");
  const std::filesystem::path p("./resources/");
  return p;
#endif
}

const std::filesystem::path getSaveFilePath() {
#ifdef BUILD_MODE_PROD
  Logger::logMsg(LogLabel::DEBUG, "Using production save file path: \"~/.local/state/wallflower/wallflower.save\" ");
  std::string homeEnv = getenv("HOME");
  const std::filesystem::path p(homeEnv + "/.local/state/wallflower/wallflower.save");
  if (!std::filesystem::exists(p)) {
    try {
      std::filesystem::create_directories(homeEnv + "/.local/state/wallflower");
      std::ofstream f(p.string());
      f.close();
      Logger::logMsg(LogLabel::DEBUG, "Save file not found. Created one in \"~/.local/state/wallflower/\"");
    } catch (const std::exception& e) {
      Logger::logMsg(LogLabel::ERROR, "Uknown error occurred: " + std::string(e.what()));
    }
  }
  return p;
#else
  Logger::logMsg(LogLabel::DEBUG, "Using development save file path: \"./resources/wallflower.save\"");
  const std::filesystem::path p("./resources/wallflower.save");
  if (!std::filesystem::exists(p)) {
    try {
      std::filesystem::create_directories("./resources");
      std::ofstream f(p.string());
      f.close();
      Logger::logMsg(LogLabel::DEBUG, "Save file not found. Created one in \"./resources\"");
    } catch (const std::exception& e) {
      Logger::logMsg(LogLabel::ERROR, "Uknown error occurred: " + std::string(e.what()));
    }
  }
  return p;
#endif
}
} // namespace Utils
