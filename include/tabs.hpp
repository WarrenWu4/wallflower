#pragma once

#include "settings.hpp"
#include "simplified_view.hpp"
#include "wallpapers.hpp"
#include "clay.h"
#include "raylib.h"
#include <memory>
#include <vector>

enum class TabType { Gallery, Settings, Simplified };

class Tabs {
public:
const int numTabs = 3;
const std::vector<std::string> tabData = {
  "Gallery",
  "Settings",
  "Simplified"
};

Texture2D galleryIcon;
Texture2D settingsIcon;

TabType currentTab;
std::shared_ptr<Wallpapers> wp;
std::shared_ptr<Settings> settingsPtr;
std::shared_ptr<SimplifiedView> simplifiedPtr;

Tabs(TabType initTab, std::shared_ptr<Wallpapers> wp, std::shared_ptr<Settings> settingsPtr, std::shared_ptr<SimplifiedView> simplifiedPtr);
~Tabs();

void tabEl();

void bodyEl();
};


