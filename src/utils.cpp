#include "utils.hpp"
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
} // namespace Utils
