#pragma once

#include "clay.h"
#include "colors.h"
#include <string>
#include <functional>
#include <map>

class Dropdown {
public:
  // INFO: using map instead of unordered map for consistent order
  std::map<std::string, std::function<void()>> items;
  std::string active;
  bool show;

  std::string parentName;
  uint32_t parentId;

  Dropdown();

  void dropdownEl();
  void dropdownItemEl(int id, std::string name, std::function<void()> callback);
};
