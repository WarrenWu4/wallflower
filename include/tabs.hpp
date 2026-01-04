#pragma once

#include "settings.hpp"
#include "wallpapers.hpp"
#include "clay.h"
#include "colors.h"
#include "raylib.h"
#include <memory>
#include <vector>
#include <iostream>

enum class TabType { Gallery, Settings };

class Tabs {
public:
const int numTabs = 2;
const std::vector<std::string> tabData = {
  "Gallery",
  "Settings"
};

Texture2D galleryIcon;
Texture2D settingsIcon;

TabType currentTab;
std::shared_ptr<Wallpapers> wp;
std::shared_ptr<Settings> settingsPtr;

Tabs(TabType initTab, std::shared_ptr<Wallpapers> wp, std::shared_ptr<Settings> settingsPtr);
~Tabs();

void tabEl();

void bodyEl();
};


