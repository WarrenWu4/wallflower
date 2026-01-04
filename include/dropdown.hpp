#pragma once

#include "clay.h"
#include "colors.h"
#include "raylib.h"
#include <string>
#include <functional>
#include <map>
#include <iostream>

class Dropdown {
public:
  // INFO: using map instead of unordered map for consistent order
  std::map<std::string, std::function<void(void*)>> items;
  std::string active;
  bool show;

  std::string parentName;
  int parentId;

  void* data;

  Dropdown();

  void closeDropdown(); 

  void dropdownEl();
  void dropdownItemEl(int id, const std::string& name, const std::function<void(void*)>& callback);
};
