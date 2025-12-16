#pragma once

#include "wallpapers.hpp"
#include <memory>
#include <vector>

enum class TabType { Gallery, Settings };

class Tabs {
public:
const int numTabs = 2;
const std::vector<std::string> tabData = {
  "Gallery",
  "Settings"
};

TabType currentTab;
std::shared_ptr<Wallpapers> wp;

Tabs(TabType initTab, std::shared_ptr<Wallpapers> wp);

void tabEl();

void bodyEl();
};


