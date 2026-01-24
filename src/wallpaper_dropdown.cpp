#include "wallpaper_dropdown.hpp"
#include "configuration.hpp"
#include "logger.hpp"
#include "raylib.h"
#include "clay.h"
#include "colors.h"
#include "utils.hpp"

void WallpaperDropdown::callback() {
  Logger::logMsg(LogLabel::DEBUG, "Wallpaper Dropdown Callback Activated");
}

WallpaperDropdown::WallpaperDropdown(std::shared_ptr<Configuration> _config) {
  show = false;
  mousePosition = GetMousePosition();
  config = _config;
  std::filesystem::path resourcePath = Utils::getResourcePath();
  std::string resourcePathStr = resourcePath.generic_string();
  fitModeIcon = LoadTexture((resourcePathStr + "icons/fit-mode-icon.png").c_str());
  monitorIcon = LoadTexture((resourcePathStr + "icons/monitor-icon.png").c_str());
  radioButton = LoadTexture((resourcePathStr + "icons/radio-inactive-icon.png").c_str());
  radioButtonActive = LoadTexture((resourcePathStr + "icons/radio-active-icon.png").c_str());
  // load radio button icon textures
  defaultMonitor = "All";

  wallpaperPath = "";
}

WallpaperDropdown::~WallpaperDropdown() {
  UnloadTexture(fitModeIcon);
  UnloadTexture(monitorIcon);
  UnloadTexture(radioButton);
  UnloadTexture(radioButtonActive);
  Logger::logMsg(LogLabel::OK, "Destructor ran");
}

void WallpaperDropdown::open() { 
  mousePosition = GetMousePosition();
  show = true; 
}

void WallpaperDropdown::close() { 
  Logger::logMsg(LogLabel::DEBUG, "Closing config menu");
  mousePosition = GetMousePosition();
  show = false; 
}

void WallpaperDropdown::toggle() {
  mousePosition = GetMousePosition();
  show = !show;
}

void WallpaperDropdown::dropdownEl() {
  if (show) {
    CLAY(CLAY_ID("WallpaperDropdownContainer"), {
      .layout = {
        .sizing = { .width = CLAY_SIZING_FIT(), .height = CLAY_SIZING_FIT() },
        .padding = {12, 12, 12, 12},
        .childGap = 8,
        .layoutDirection = CLAY_TOP_TO_BOTTOM
      },
      .backgroundColor = COLOR_BACKGROUND_2,
      .floating = { 
        .offset = { mousePosition.x, mousePosition.y },
        .attachTo = CLAY_ATTACH_TO_ROOT
      },
    }) {
      fitModeEl();
      monitorsEl();
    }
  }
}

void WallpaperDropdown::fitModeEl() {
  CLAY(CLAY_ID("WallpaperDropdownHeaderFitMode"), {
    .layout = {
      .sizing = {
        .width = CLAY_SIZING_GROW(),
        .height = CLAY_SIZING_FIT()
      },
      .childGap = 4,
      .childAlignment = {
        .y = CLAY_ALIGN_Y_CENTER
      },
    }
  }) {
    CLAY(CLAY_ID("WallpaperDropdownFitModeIcon"), {
      .layout = {
        .sizing = { .width = CLAY_SIZING_FIXED(12), .height = CLAY_SIZING_FIXED(12) },
      },
      .image = {.imageData = &fitModeIcon}
    });
    CLAY_TEXT(
      CLAY_STRING("Fit Mode"),
      CLAY_TEXT_CONFIG({
        .textColor = COLOR_FOREGROUND_3,
        .fontSize = 20
      })
    );
  }

  CLAY(CLAY_ID("WallpaperDropdownFitMode"), {
    .layout = {
      .sizing = {
        .width = CLAY_SIZING_GROW(),
        .height = CLAY_SIZING_FIT()
      },
      .layoutDirection = CLAY_TOP_TO_BOTTOM
    }
  }) {
    for (size_t i = 0; i < modeToStringUpper.size(); i++) {
      const std::unordered_map<std::string, WallpaperData>& temp = config->getConfig().preferences;
      if (temp.contains(wallpaperPath)) {
        fitModeOptionEl(i, modeToStringUpper.at(i), temp.at(wallpaperPath).fitMode == static_cast<FitMode>(i));
      } else {
        fitModeOptionEl(i, modeToStringUpper.at(i), FitMode::COVER == static_cast<FitMode>(i));
      }
    }
  }
}

void WallpaperDropdown::fitModeOptionEl(int id, const std::string& fitMode, bool selected) {
  CLAY(CLAY_IDI("WallpaperDropdownFitModeOption", id), {
    .layout = {
      .sizing = {
        .width = CLAY_SIZING_GROW(),
        .height = CLAY_SIZING_FIT()
      },
      .childGap = 4,
      .childAlignment = {
        .y = CLAY_ALIGN_Y_CENTER
      }
    }
  }) {
    if (Clay_Hovered() && IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
      Logger::logMsg(LogLabel::DEBUG, "Updating fit mode preference for image");
      const std::unordered_map<std::string, WallpaperData>& temp = config->getConfig().preferences;
      WallpaperData wd = {
        .path = wallpaperPath,
        .fitMode = stringToFitMode.at(Utils::toLowerString(fitMode)),
        .monitor = ""
      };
      if (temp.contains(wallpaperPath)) {
        wd.monitor = temp.at(wallpaperPath).monitor;
      }
      config->addPreferences({wd}, true);
    }
    CLAY(CLAY_IDI("WallpaperDropdownFitModeOptionIcon", id), {
      .layout = {
        .sizing = { .width = CLAY_SIZING_FIXED(12), .height = CLAY_SIZING_FIXED(12) },
      },
      .image = {.imageData = (selected) ? &radioButtonActive : &radioButton }
    });
    CLAY_TEXT(
      Clay_String({
        .length = static_cast<int32_t>(fitMode.size()),
        .chars = fitMode.c_str()
      }),
      CLAY_TEXT_CONFIG({
        .textColor = COLOR_FOREGROUND_3,
        .fontSize = 20
      })
    );
  }
}

void WallpaperDropdown::monitorsEl() {
  CLAY(CLAY_ID("WallpaperDropdownHeader"), {
    .layout = {
      .sizing = {
        .width = CLAY_SIZING_GROW(),
        .height = CLAY_SIZING_FIT()
      },
      .childGap = 4,
      .childAlignment = {
        .y = CLAY_ALIGN_Y_CENTER
      }
    }
  }) {
    CLAY(CLAY_ID("WallpaperDropdownMonitorIcon"), {
      .layout = {
        .sizing = { .width = CLAY_SIZING_FIXED(12), .height = CLAY_SIZING_FIXED(12) },
      },
      .image = {.imageData = &monitorIcon }
    });
    CLAY_TEXT(
      CLAY_STRING("Monitors"),
      CLAY_TEXT_CONFIG({
        .textColor = COLOR_FOREGROUND_3,
        .fontSize = 20
      })
    );
  }

  CLAY(CLAY_ID("WallpaperDropdownMonitors"), {
    .layout = {
      .sizing = {
        .width = CLAY_SIZING_GROW(),
        .height = CLAY_SIZING_GROW()
      },
      .layoutDirection = CLAY_TOP_TO_BOTTOM
    }
  }) {
    const std::vector<MonitorInfo>& monitors = config->getMonitors();
    monitorOptionEl(monitors.size()+1, defaultMonitor, true);
    for(size_t i = 0; i < monitors.size(); i++) {
      const std::unordered_map<std::string, WallpaperData>& temp = config->getConfig().preferences; 
      if (temp.contains(wallpaperPath)) {
        monitorOptionEl(i, monitors.at(i).name, monitors.at(i).name == temp.at(wallpaperPath).monitor);
      } else {
        monitorOptionEl(i, monitors.at(i).name, monitors.at(i).name == "All");
      }
    }
  }
}

void WallpaperDropdown::monitorOptionEl(int id, const std::string& name, bool selected) {
  CLAY(CLAY_IDI("WallpaperDropdownMonitorOption", id), {
    .layout = {
      .sizing = {
        .width = CLAY_SIZING_GROW(),
        .height = CLAY_SIZING_FIT()
      },
      .childGap = 4,
      .childAlignment = {
        .y = CLAY_ALIGN_Y_CENTER
      }
    }
  }) {
    if (Clay_Hovered() && IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
      Logger::logMsg(LogLabel::DEBUG, "Updating monitor preference for image");
      // TODO: update monitor preferences and update wallpaper
      // const WallflowerConfig& tempConfig = config->getConfig();
      // config->updateWallpaper((name == "All") ? "" : name, )
    }
    CLAY(CLAY_IDI("WallpaperDropdownMonitorOptionIcon", id), {
      .layout = {
        .sizing = { .width = CLAY_SIZING_FIXED(12), .height = CLAY_SIZING_FIXED(12) },
      },
      .image = {.imageData = (selected) ? &radioButtonActive : &radioButton }
    });
    CLAY_TEXT(
      Clay_String({
        .length = static_cast<int32_t>(name.size()),
        .chars = name.c_str()
      }),
      CLAY_TEXT_CONFIG({
        .textColor = COLOR_FOREGROUND_3,
        .fontSize = 20
      })
    );
  }
}
