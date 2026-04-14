#include "raylib.h"
#include "simplified_view.hpp"
#include "utils.hpp"
#include "wallpaper_dropdown.hpp"
#define CLAY_IMPLEMENTATION
#include "clay.h"
#include "clay_renderer_raylib.c"
#include "colors.h"
#include "wallpapers.hpp"
#include "tabs.hpp"
#include "configuration.hpp"
#include "logger.hpp"
#include "wallflower/views/main_view.hpp"

#include <memory>


void HandleClayErrors(Clay_ErrorData errorData) {
  printf("%s", errorData.errorText.chars);
}

int main() {
  // init raylib
  Logger::logMsg(LogLabel::DEBUG, "Initializing Raylib");
  SetConfigFlags(FLAG_VSYNC_HINT | FLAG_MSAA_4X_HINT | FLAG_WINDOW_HIGHDPI);
  const float screenWidth = 800.0f;
  const float screenHeight = 600.0f;
  Clay_Raylib_Initialize((int)screenWidth, (int)screenHeight, "Wallflower", 0);
  SetTargetFPS(60);

  // init clay
  Logger::logMsg(LogLabel::DEBUG, "Initializing Clay");
  uint64_t clayMemorySize = Clay_MinMemorySize();
  std::unique_ptr<char[]> clayMemory(new char[clayMemorySize]);
  Clay_Arena arena = Clay_CreateArenaWithCapacityAndMemory(clayMemorySize, clayMemory.get());
  Clay_Initialize(arena, (Clay_Dimensions){screenWidth, screenHeight}, (Clay_ErrorHandler){HandleClayErrors});
  const float scrollSpeedMultiplier = 8.0f;

  // init fonts
  Logger::logMsg(LogLabel::DEBUG, "Initializing Fonts");
  std::filesystem::path resourcePath = Utils::getResourcePath();
  Font fontMontserratBold = LoadFont((resourcePath.generic_string() + "fonts/Montserrat-Bold.ttf").c_str());
  Font fontMontserratSemiBold = LoadFont((resourcePath.generic_string() + "fonts/Montserrat-SemiBold.ttf").c_str());
  SetTextureFilter(fontMontserratSemiBold.texture, TEXTURE_FILTER_BILINEAR);
  SetTextureFilter(fontMontserratBold.texture, TEXTURE_FILTER_BILINEAR);
  Clay_SetMeasureTextFunction(Raylib_MeasureText, &fontMontserratSemiBold);

  // init configuration
  Logger::logMsg(LogLabel::DEBUG, "Initializing Configuration");
  std::shared_ptr<Configuration> configuration = std::make_shared<Configuration>();

  // init main view
  std::unique_ptr<MainView> mainView = std::make_unique<MainView>();

  while (!WindowShouldClose()) {
    Vector2 mousePosition = GetMousePosition();
    Vector2 scrollDelta = GetMouseWheelMoveV();
    float deltaTime = GetFrameTime();

    Clay_SetPointerState({mousePosition.x, mousePosition.y}, IsMouseButtonDown(MOUSE_LEFT_BUTTON));
    Clay_SetLayoutDimensions({(float)GetScreenWidth(), (float)GetScreenHeight()});
    Clay_UpdateScrollContainers(true, (Clay_Vector2) {scrollDelta.x * scrollSpeedMultiplier, scrollDelta.y * scrollSpeedMultiplier}, deltaTime);

    Clay_BeginLayout();
    mainView->view();
    Clay_RenderCommandArray renderCommands = Clay_EndLayout();

    BeginDrawing();
    Clay_Raylib_Render(renderCommands, &fontMontserratSemiBold);
    EndDrawing();
  }

  // TODO: probably offload this crap to a font manager class for automatic frees
  // unload fonts
  UnloadFont(fontMontserratSemiBold);
  UnloadFont(fontMontserratBold);

  Logger::logMsg(LogLabel::OK, "Ending wallflower");
  return 0;
}
