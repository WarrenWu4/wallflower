#include "bouncer.hpp"
#include "raylib.h"
#include "utils.hpp"
#include <exception>
#define CLAY_IMPLEMENTATION
#include "clay.h"
#include "clay_renderer_raylib.c"
#include "colors.h"
#include "wallpapers.hpp"
#include "tabs.hpp"
#include "configuration.hpp"
#include "logger.hpp"
#include "dropdown.hpp"

#include <memory>


void HandleClayErrors(Clay_ErrorData errorData) {
  // See the Clay_ErrorData struct for more information
  printf("%s", errorData.errorText.chars);
  // switch (errorData.errorType) {
  //   // etc
  // }
}

void handleDropdownFitMode(std::shared_ptr<Configuration> configuration, std::shared_ptr<Dropdown> dropdownFitMode, void* path, FitMode fitMode) {
  if (path == nullptr) {
    Logger::logMsg(LogLabel::ERROR, "No image path provided to dropdown");
    return; 
  }
  try {
    std::string pathStr = *static_cast<std::string*>(path);
    std::string monitor = "";
    if (configuration->getConfig().preferences.find(pathStr) != configuration->getConfig().preferences.end()) {
      monitor = configuration->getConfig().preferences.find(pathStr)->second.monitor;
    }
    configuration->addPreferences({(WallpaperData) {
      .path = pathStr,
      .fitMode = fitMode,
      .monitor = monitor
    }});
    Logger::logMsg(LogLabel::OK, "Updated fit mode to " + modeToStringUpper.at(static_cast<int>(fitMode)) + " for image " + pathStr);
    dropdownFitMode->closeDropdown();
  } catch (const std::exception& e) {
    Logger::logMsg(LogLabel::FAIL, "Failed to updated fit mode: " + std::string(e.what()));
  }
}

int main() {
  // init raylib
  SetConfigFlags(FLAG_VSYNC_HINT | FLAG_MSAA_4X_HINT | FLAG_WINDOW_HIGHDPI);
  const float screenWidth = 800.0f;
  const float screenHeight = 600.0f;
  Clay_Raylib_Initialize((int)screenWidth, (int)screenHeight, "Wallflower", 0);
  SetTargetFPS(60);

  // init clay
  uint64_t clayMemorySize = Clay_MinMemorySize();
  std::unique_ptr<char[]> clayMemory(new char[clayMemorySize]);
  Clay_Arena arena =
      Clay_CreateArenaWithCapacityAndMemory(clayMemorySize, clayMemory.get());
  Clay_Initialize(arena, (Clay_Dimensions){screenWidth, screenHeight},
                  (Clay_ErrorHandler){HandleClayErrors});

  // init fonts
  std::filesystem::path resourcePath = Utils::getResourcePath();
  Font fontMontserratBold = LoadFont((resourcePath.generic_string() + "fonts/Montserrat-Bold.ttf").c_str());
  Font fontMontserratSemiBold = LoadFont((resourcePath.generic_string() + "fonts/Montserrat-SemiBold.ttf").c_str());
  SetTextureFilter(fontMontserratSemiBold.texture, TEXTURE_FILTER_BILINEAR);
  SetTextureFilter(fontMontserratBold.texture, TEXTURE_FILTER_BILINEAR);
  Clay_SetMeasureTextFunction(Raylib_MeasureText, &fontMontserratSemiBold);

  Logger::logMsg(LogLabel::DEBUG, "Initializing program objects");

  std::shared_ptr<Bouncer> bouncer = std::make_shared<Bouncer>();
  std::shared_ptr<Configuration> configuration = std::make_shared<Configuration>();

  std::shared_ptr<Settings> settings = std::make_shared<Settings>(configuration);

  std::shared_ptr<Dropdown> dropdownFitMode = std::make_shared<Dropdown>();
  dropdownFitMode->items.insert(
    {"CONTAIN", [dropdownFitMode, configuration](void* data) {
    handleDropdownFitMode(configuration, dropdownFitMode, data, FitMode::CONTAIN);
    }}
  );
  dropdownFitMode->items.insert(
    {"COVER", [dropdownFitMode, configuration](void* data) {
    handleDropdownFitMode(configuration, dropdownFitMode, data, FitMode::COVER);
    }}
  );
  
  std::shared_ptr<Wallpapers> wp = std::make_shared<Wallpapers>(configuration, settings, dropdownFitMode);

  std::shared_ptr<Tabs> tabs = std::make_shared<Tabs>(TabType::Gallery, wp, settings);

  // register callbacks
  configuration->callbackAddDirectory.push_back([&]() {
    wp->onSearchPathChange(true);
  });

  configuration->callbackRemoveDirectory.push_back([&]() {
    wp->onSearchPathChange(false);
  });

  Logger::logMsg(LogLabel::DEBUG, "Finished initializing program objects");

  const float scrollSpeedMultiplier = 8.0f;

  // loop
  while (!WindowShouldClose()) {
    // update state
    Vector2 mousePosition = GetMousePosition();
    Vector2 scrollDelta = GetMouseWheelMoveV();
    float deltaTime = GetFrameTime();

    Clay_SetPointerState({mousePosition.x, mousePosition.y}, IsMouseButtonDown(MOUSE_LEFT_BUTTON));
    Clay_SetLayoutDimensions(
        {(float)GetScreenWidth(), (float)GetScreenHeight()});
    Clay_UpdateScrollContainers(true, (Clay_Vector2) {scrollDelta.x * scrollSpeedMultiplier, scrollDelta.y * scrollSpeedMultiplier}, deltaTime);

    // build UI
    Clay_BeginLayout();

    CLAY(CLAY_ID("MainContainer"), {
        .layout = {
          .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
          .padding = {CLAY_PADDING_ALL(16)},
          .childGap = 16,
          .layoutDirection = CLAY_TOP_TO_BOTTOM
        },
        .backgroundColor = COLOR_BACKGROUND_1,
    }) {
      tabs->tabEl();
      tabs->bodyEl();
      dropdownFitMode->dropdownEl();
    }
    Clay_RenderCommandArray renderCommands = Clay_EndLayout();

    // render
    BeginDrawing();
    Clay_Raylib_Render(renderCommands, &fontMontserratSemiBold);
    EndDrawing();
  }

  UnloadFont(fontMontserratSemiBold);
  UnloadFont(fontMontserratBold);

  // manual call destructor since it doesnt seem to be automatically called
  configuration->~Configuration();

  return 0;
}
