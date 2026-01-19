#include "wallpaper_dropdown.hpp"
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
  monitorIcon = LoadTexture((resourcePath.generic_string() + "icons/monitor-icon.png").c_str());
  // load monitor icon texture
}

WallpaperDropdown::~WallpaperDropdown() {
  UnloadTexture(monitorIcon);
}

void WallpaperDropdown::open() { 
  mousePosition = GetMousePosition();
  show = true; 
}

void WallpaperDropdown::close() { 
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
        .sizing = { .width = CLAY_SIZING_FIT(), .height = CLAY_SIZING_FIXED(120) },
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
        for(size_t i = 0; i < 2; i++) {
          CLAY_TEXT(
            CLAY_STRING("Monitor Name"),
            CLAY_TEXT_CONFIG({
              .textColor = COLOR_FOREGROUND_1,
              .fontSize = 20 
            })
          );
        }
      }

    }
  }
}
