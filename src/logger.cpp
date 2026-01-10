#include "logger.hpp"
#include <chrono>
#include <cstring>
#include <filesystem>
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

#ifdef BUILD_MODE_PROD
FileLogger fl = FileLogger();
#endif

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

  std::string log = getColorCode(level) + "[" + timeStr + "] " + "[" +
                    levelStr + "] " + "[" + src + "] " + msg + ansiReset + "\n";
#ifdef BUILD_MODE_PROD
  fl.write(log);
#else
  std::cout << log;
#endif
}
} // namespace Logger

FileLogger::FileLogger() {
  const std::string path =
      std::string(getenv("HOME")) + "/.local/state/wallflower";
  const std::filesystem::path p(FILE_NAME);
  if (!std::filesystem::exists(p)) {
    try {
      std::filesystem::create_directories(path);
      std::ofstream f(FILE_NAME);
      f.close();
    } catch (const std::exception &e) {
      throw std::runtime_error("Unable to create log file: " +
                               std::string(e.what()));
    }
  }
  file.open(FILE_NAME, std::ios::binary | std::ios::in | std::ios::out | std::ios::ate);
  if (!file) {
    throw std::runtime_error("Cannot open log file at: " + FILE_NAME);
  }
  buffer.resize(BUFFER_SIZE);
  bufferSize = 0;
  temp.resize(MAX_FILE_SIZE);
}

FileLogger::~FileLogger() {
  if (file.is_open()) {
    flush();
    file.close();
  }
}

void FileLogger::write(const std::string &data) {
  std::size_t dataSize = data.size();
  if (static_cast<int>(bufferSize + dataSize) > BUFFER_SIZE) {
    flush();
  }
  std::memcpy(buffer.data() + bufferSize, data.data(), dataSize);
  bufferSize += dataSize;
}

void FileLogger::flush() {
  int currFileSize = file.tellg();
  if (bufferSize + currFileSize > MAX_FILE_SIZE) {
    int bytesToRemove = bufferSize + currFileSize - MAX_FILE_SIZE;
    int oldDataSize = currFileSize - bytesToRemove;
    file.seekg(bytesToRemove, std::ios::beg);
    file.read(temp.data(), oldDataSize);
    file.seekp(0, std::ios::beg);
    file.write(temp.data(), oldDataSize);
  }
  file.write(buffer.data(), bufferSize);
  bufferSize = 0;
}
