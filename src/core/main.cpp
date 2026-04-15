#include "raylib.h"
#define CLAY_IMPLEMENTATION
#include "clay.h"
#include "clay_renderer_raylib.c"
#include "colors.h"
#include "../data_managers/configuration.hpp"
#include "../utils/utils.hpp"
#include "../utils/logger.hpp"
#include "../views/main/main.hpp"

#include <memory>
#include <cstdio>

void HandleClayErrors(Clay_ErrorData errorData) {
  printf("%s", errorData.errorText.chars);
}

int main() {
  Logger::logMsg(LogLabel::DEBUG, "Initializing Raylib");
  SetConfigFlags(FLAG_VSYNC_HINT | FLAG_MSAA_4X_HINT | FLAG_WINDOW_HIGHDPI);
  const float screenWidth = 800.0f;
  const float screenHeight = 600.0f;
  Clay_Raylib_Initialize((int)screenWidth, (int)screenHeight, "Wallflower", 0);
  SetTargetFPS(60);

  Logger::logMsg(LogLabel::DEBUG, "Initializing Clay");
  uint64_t clayMemorySize = Clay_MinMemorySize();
  std::unique_ptr<char[]> clayMemory(new char[clayMemorySize]);
  Clay_Arena arena = Clay_CreateArenaWithCapacityAndMemory(clayMemorySize, clayMemory.get());
  Clay_Initialize(arena, (Clay_Dimensions){screenWidth, screenHeight}, (Clay_ErrorHandler){HandleClayErrors});

  std::filesystem::path resourcePath = Utils::getResourcePath();
  Font fontMontserratSemiBold = LoadFont((resourcePath.generic_string() + "fonts/Montserrat-SemiBold.ttf").c_str());
  SetTextureFilter(fontMontserratSemiBold.texture, TEXTURE_FILTER_BILINEAR);
  Clay_SetMeasureTextFunction(Raylib_MeasureText, &fontMontserratSemiBold);

  std::shared_ptr<Configuration> configuration = std::make_shared<Configuration>();
  AppModel appState = { TAB_GALLERY, "", false, FitMode::COVER };

  while (!WindowShouldClose()) {
    Vector2 mousePosition = GetMousePosition();
    Vector2 scrollDelta = GetMouseWheelMoveV();
    float deltaTime = GetFrameTime();

    Clay_SetPointerState({mousePosition.x, mousePosition.y}, IsMouseButtonDown(MOUSE_LEFT_BUTTON));
    Clay_SetLayoutDimensions({(float)GetScreenWidth(), (float)GetScreenHeight()});
    Clay_UpdateScrollContainers(true, (Clay_Vector2) {scrollDelta.x * 8.0f, scrollDelta.y * 8.0f}, deltaTime);

    Clay_BeginLayout();
    AppMsg msg = Main_View(&appState);
    appState = Main_Update(appState, msg);
    Clay_RenderCommandArray renderCommands = Clay_EndLayout();

    BeginDrawing();
    Clay_Raylib_Render(renderCommands, &fontMontserratSemiBold);
    EndDrawing();
  }

  UnloadFont(fontMontserratSemiBold);
  return 0;
}
