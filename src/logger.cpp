#include "logger.hpp"
#include <chrono>
#include <iostream>

namespace Logger {
namespace {
const std::string ansiReset = "\033[0m";
std::string getColorCode(LogLabel level) {
  switch (level) {
  case LogLabel::DEBUG:
    return "\033[35m"; // purple
  case LogLabel::ERROR:
    return "\033[31m"; // red
  case LogLabel::WARNING:
    return "\033[33m"; // yellow
  case LogLabel::OK:
    return "\033[32m"; // green
  case LogLabel::FAIL:
    return "\033[31m"; // red
  default:
    return "\033[0m";
  }
}

std::string levelToString(LogLabel level) {
  switch (level) {
  case LogLabel::DEBUG:
    return "DEBUG";
  case LogLabel::WARNING:
    return "WARNING";
  case LogLabel::ERROR:
    return "ERROR";
  case LogLabel::OK:
    return "OK";
  case LogLabel::FAIL:
    return "FAIL";
  default:
    return "UNKNOWN";
  }
}

std::string getTimestamp() {
  auto now = std::chrono::system_clock::now();
  auto in_time_t = std::chrono::system_clock::to_time_t(now);

  std::stringstream ss;
  ss << std::put_time(std::localtime(&in_time_t), "%Y-%m-%d %H:%M:%S");
  return ss.str();
}
} // namespace
void logMsg(LogLabel level, const std::string &msg,
            const std::source_location location) {
  std::string timeStr = getTimestamp();
  std::string levelStr = levelToString(level);

  // Extract file name and line (similar to getCallerLocation)
  std::string src =
      std::string(location.file_name()) + ":" + std::to_string(location.line());

  std::cout << getColorCode(level) << "[" << timeStr << "] "
            << "[" << levelStr << "] "
            << "[" << src << "] " << msg << ansiReset << std::endl;
}
} // namespace Logger
