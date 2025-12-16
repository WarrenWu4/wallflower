#include "raylib.h"
#define CLAY_IMPLEMENTATION
#include "clay.h"
#include "clay_renderer_raylib.c"

#include <iostream>
#include <memory>

// Function to handle the button click logic
void HandleButtonClick() {
  std::cout << "[System]: Button Clicked!" << std::endl;
  system("echo 'Hello from Clay + Raylib!'");
}

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

  // loop
  while (!WindowShouldClose()) {
    // update state
    Vector2 mousePosition = GetMousePosition();
    Vector2 scrollDelta = GetMouseWheelMoveV();

    Clay_SetPointerState({mousePosition.x, mousePosition.y},
                         IsMouseButtonDown(MOUSE_LEFT_BUTTON));
    Clay_SetLayoutDimensions(
        {(float)GetScreenWidth(), (float)GetScreenHeight()});

    // build UI
    Clay_BeginLayout();

    CLAY(CLAY_ID("MainContainer"), {
        .layout = {
          .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
          .padding = {CLAY_PADDING_ALL(0)},
          .childGap = 16
        },
        .backgroundColor = {44, 52, 50, 255},
    }) {
      CLAY(CLAY_ID("EchoButton"), {
        .layout = {
          .sizing = {
            .width = CLAY_SIZING_FIXED(200),
            .height = CLAY_SIZING_FIXED(40)
          },
          .padding = {20, 10}
        },
        .backgroundColor = {255, 255, 255, 255}
      }) {
        if (Clay_PointerOver(Clay_GetElementId(CLAY_STRING("EchoButton"))) && IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
          HandleButtonClick();
        }
        CLAY_TEXT(CLAY_STRING("Test Button"), CLAY_TEXT_CONFIG(
          { 
            .textColor = {0, 0, 0, 255},
            .fontSize = 24 
          }));
      }
    }
    Clay_RenderCommandArray renderCommands = Clay_EndLayout();

    // render
    BeginDrawing();
    Clay_Raylib_Render(renderCommands, &fontMontserrat);
    EndDrawing();
  }

  return 0;
}
