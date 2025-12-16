#pragma once

#include "settings.hpp"
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
std::shared_ptr<Settings> settingsPtr;

Tabs(TabType initTab, std::shared_ptr<Wallpapers> wp, std::shared_ptr<Settings> settingsPtr);

void tabEl();

void bodyEl();
};


