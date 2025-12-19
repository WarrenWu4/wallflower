#include "raylib.h"
#define CLAY_IMPLEMENTATION
#include "clay.h"
#include "clay_renderer_raylib.c"
#include "colors.h"
#include "wallpapers.hpp"
#include "tabs.hpp"
#include "configuration.hpp"
#include "dropdown.hpp"

#include <memory>
#include <iostream>

void HandleClayErrors(Clay_ErrorData errorData) {
  // See the Clay_ErrorData struct for more information
  printf("%s", errorData.errorText.chars);
  // switch (errorData.errorType) {
  //   // etc
  // }
}

int main() {
  // init raylib
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
  Font fontMontserratBold = LoadFont("resources/fonts/Montserrat-Bold.ttf");
  Font fontMontserratSemiBold = LoadFont("resources/fonts/Montserrat-SemiBold.ttf");
  SetTextureFilter(fontMontserratSemiBold.texture, TEXTURE_FILTER_BILINEAR);
  SetTextureFilter(fontMontserratBold.texture, TEXTURE_FILTER_BILINEAR);

  Clay_SetMeasureTextFunction(Raylib_MeasureText, &fontMontserratSemiBold);

  std::shared_ptr<Configuration> configuration = std::make_shared<Configuration>();

  std::shared_ptr<Settings> settings = std::make_shared<Settings>(configuration);

  std::shared_ptr<Dropdown> dropdownFitMode = std::make_shared<Dropdown>();
  dropdownFitMode->items.insert(
    {"CONTAIN", [configuration](void* data) {
      if (data == nullptr) { 
        std::cerr << "No image path provided\n";
        return; 
      }
      std::string path = *static_cast<std::string*>(data);
      configuration->imageData[path] = WallpaperMode::CONTAIN;
      runHyprCommand("", path, WallpaperMode::CONTAIN);
      std::cout << "set fit mode to contain\n";
    }}
  );
  dropdownFitMode->items.insert(
    {"COVER", [configuration](void* data) {
      if (data == nullptr) { 
        std::cerr << "No image path provided\n";
        return; 
      }
      std::string path = *static_cast<std::string*>(data);
      configuration->imageData[path] = WallpaperMode::CONTAIN;
      runHyprCommand("", path, WallpaperMode::CONTAIN);
      std::cout << "set fit mode to cover\n";
    }}
  );
  
  std::shared_ptr<Wallpapers> wp = std::make_shared<Wallpapers>(configuration, settings, dropdownFitMode); 

  std::shared_ptr<Tabs> tabs = std::make_shared<Tabs>(TabType::Gallery, wp, settings);

  // loop
  while (!WindowShouldClose()) {
    // update state
    Vector2 mousePosition = GetMousePosition();
    Vector2 scrollDelta = GetMouseWheelMoveV();

    Clay_SetPointerState({mousePosition.x, mousePosition.y}, IsMouseButtonDown(MOUSE_LEFT_BUTTON));
    Clay_SetLayoutDimensions(
        {(float)GetScreenWidth(), (float)GetScreenHeight()});
    Clay_UpdateScrollContainers(true, (Clay_Vector2) {scrollDelta.x, scrollDelta.y}, 0.00016f);

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

  return 0;
}
