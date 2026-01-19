#include "wallpaper_dropdown.hpp"
#include "logger.hpp"
#include "raylib.h"
#include "clay.h"
#include "colors.h"

void WallpaperDropdown::callback() {
  Logger::logMsg(LogLabel::DEBUG, "Wallpaper Dropdown Callback Activated");
}

WallpaperDropdown::WallpaperDropdown(std::shared_ptr<Configuration> _config) {
  show = false;
  mousePosition = GetMousePosition();
  config = _config;
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
        .sizing = { .width = CLAY_SIZING_FIXED(80), .height = CLAY_SIZING_FIXED(120) },
      },
      .backgroundColor = COLOR_BACKGROUND_2,
      .floating = { 
        .offset = { mousePosition.x, mousePosition.y },
        .attachTo = CLAY_ATTACH_TO_ROOT
      },
    });
  }
}
