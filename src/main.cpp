#include "raylib.h"
#define CLAY_IMPLEMENTATION
#include "clay.h"
#include "clay_renderer_raylib.c"
#include "colors.h"
#include "wallpapers.hpp"

#include <memory>

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

  // init fonts
  Font fontMontserrat =
      LoadFontEx("resources/Montserrat-VariableFont_wght.ttf", 32, 0, 250);

  // init clay
  uint64_t clayMemorySize = Clay_MinMemorySize();
  std::unique_ptr<char[]> clayMemory(new char[clayMemorySize]);
  Clay_Arena arena =
      Clay_CreateArenaWithCapacityAndMemory(clayMemorySize, clayMemory.get());
  Clay_Initialize(arena, (Clay_Dimensions){screenWidth, screenHeight},
                  (Clay_ErrorHandler){HandleClayErrors});

  Clay_SetMeasureTextFunction(Raylib_MeasureText, &fontMontserrat);
  
  Wallpapers wp = Wallpapers(); 
  wp.scanDirectory("/home/warrenwu/backgrounds/memes");
  wp.addWallpaper("/home/warrenwu/backgrounds/depresso.png");

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
          .childGap = 16
        },
        .backgroundColor = COLOR_BACKGROUND_1,
    }) {
      CLAY(CLAY_ID("ContentContainer"), {
        .layout = {
          .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
        }
      }) {
        CLAY(CLAY_ID("ScrollContainer"), {
          .layout = { 
            .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
          },
          .clip = { .vertical = true, .childOffset = Clay_GetScrollOffset() }
        }) {
          wp.wallpaperContainerEl();
        }
      }
    }
    Clay_RenderCommandArray renderCommands = Clay_EndLayout();

    // render
    BeginDrawing();
    Clay_Raylib_Render(renderCommands, &fontMontserrat);
    EndDrawing();
  }

  UnloadFont(fontMontserrat);

  return 0;
}
