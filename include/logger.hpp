#pragma once

#include <fstream>
#include <source_location>
#include <string>
#include <vector>

/**
 * DEBUG -> Useful information helpful in debugging process
 *
 * WARNING -> Issue that does not critically impact program behavior
 *
 * ERROR -> Issue that results in unknown behaviors
 *
 * OK -> Successful execution of event
 *
 * FAIL -> Unsuccessful execution of event
 *
 *
 * FAIL vs. ERROR -- why have two erroneous labels?
 * FAIL is designed to be handled within the bounds of the program
 * ERROR is issues with external factors beyond the program
 */
enum class LogLabel { DEBUG, WARNING, ERROR, OK, FAIL };

namespace Logger {
void logMsg(LogLabel level, const std::string &msg,
            const std::source_location = std::source_location().current());
}

class FileLogger {
private:
  const int BUFFER_SIZE = 16 * 1024;
  const size_t MAX_FILE_SIZE = 100 * 1024;
  const std::string FILE_NAME = std::string(getenv("HOME")) + "/.local/state/wallflower/wallflower.log";
  std::fstream file;
  std::vector<char> buffer;
  std::size_t bufferSize;
  std::vector<char> temp;
public:
  FileLogger();
  ~FileLogger();
  void write(const std::string &data);
  void flush();
};
