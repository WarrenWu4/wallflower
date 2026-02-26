#pragma once

#include "clay.h"
#include "raylib.h"
#include "settings.hpp"
#include "simplified_view.hpp"
#include "wallpapers.hpp"
#include <memory>
#include <vector>

enum class TabType { Gallery, Settings, Simplified };

struct MenuButtonData {
  Texture2D icon;
  bool showDropdown;
};

struct TabData {
  TabType type;
  std::string name;
  std::string iconPath;
  Texture2D icon;
};

class Tabs {
private:
  std::vector<TabData> tabsData;
  void loadTabsData(std::filesystem::path resourcePath);
  void unloadTabsData();

  MenuButtonData menuButtonData;
  void loadMenuButtonData(std::filesystem::path resourcePath);
  void unloadMenuButtonData();
  void menuButtonEl();

public:
  const int numTabs = 3;
  const std::vector<std::string> tabData = {"Gallery", "Settings", "Simplified"};

  Texture2D galleryIcon;
  Texture2D settingsIcon;

  TabType currentTab;
  std::shared_ptr<Wallpapers> wp;
  std::shared_ptr<Settings> settingsPtr;
  std::shared_ptr<SimplifiedView> simplifiedPtr;

  Tabs(TabType initTab, std::shared_ptr<Wallpapers> wp,
       std::shared_ptr<Settings> settingsPtr,
       std::shared_ptr<SimplifiedView> simplifiedPtr);
  ~Tabs();

  void tabEl();

  void bodyEl();
};
