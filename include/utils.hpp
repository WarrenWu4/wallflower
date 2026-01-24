#pragma once

#include <filesystem>
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

/*
 * gets the path of the resource folder
 */
const std::filesystem::path getResourcePath();

/*
 * get path of the save file
 * creates save file if not exists
 */
const std::filesystem::path getSaveFilePath();

/*
 * Checks path is a valid hyprpaper wallpaper path
 *
 * In order to be valid the following has to be true:
 * 1. file has to exist on the system
 * 2. path must start with '~' or '/'
 */
bool isValidHyprPath(std::string_view imagePath);

/*
 * returns all uppercase version of string
 */
std::string toUpperString(std::string str);

/*
 * returns all lowercase version of string
 */
std::string toLowerString(std::string str);

/*
 * returns all valid images in given path
 * currently supported image formats:
 * PNG, JPEG 
 */
std::vector<std::string> getImagesInDirectory(std::string path);
} // namespace Utils
