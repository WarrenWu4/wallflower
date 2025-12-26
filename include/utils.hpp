#pragma once

#include <string>
#include <vector>

namespace Utils {
/**
 * splits string based on delimiter character
 */
std::vector<std::string> split(const std::string &s, char d);

/**
 * removes all whitespaces in a string
 * whitespaces: spaces, new lines, and tabs
 */
std::string removeWhitespace(const std::string &s);
} // namespace Utils
