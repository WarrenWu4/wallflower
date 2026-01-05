#pragma once

#include <string>
#include <vector>
#include <filesystem>

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

/*
 * gets the path of the resource folder
 */
const std::filesystem::path getResourcePath();

/*
 * get path of the save file
 * creates save file if not exists
 */
const std::filesystem::path getSaveFilePath();
} // namespace Utils
