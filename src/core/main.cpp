#include "core/raylib.h"
#define CLAY_IMPLEMENTATION
#include "core/clay.h"
#include "core/clay_renderer_raylib.c"
#include "data_managers/configuration.hpp"
#include "utils/logger.hpp"
#include "utils/utils.hpp"
#include "views/app/app.hpp"

#include <cstdio>
#include <memory>

void HandleClayErrors(Clay_ErrorData errorData) {
    printf("%s", errorData.errorText.chars);
}

int main() {
    Logger::logMsg(LogLabel::DEBUG, "Initializing Raylib");
    SetConfigFlags(FLAG_VSYNC_HINT | FLAG_MSAA_4X_HINT | FLAG_WINDOW_HIGHDPI);
    const float screenWidth = 800.0f;
    const float screenHeight = 600.0f;
    Clay_Raylib_Initialize((int)screenWidth, (int)screenHeight, "Wallflower",
                           0);
    SetTargetFPS(60);

    Logger::logMsg(LogLabel::DEBUG, "Initializing Clay");
    uint64_t clayMemorySize = Clay_MinMemorySize();
    std::unique_ptr<char[]> clayMemory(new char[clayMemorySize]);
    Clay_Arena arena =
        Clay_CreateArenaWithCapacityAndMemory(clayMemorySize, clayMemory.get());
    Clay_Initialize(arena, (Clay_Dimensions){screenWidth, screenHeight},
                    (Clay_ErrorHandler){HandleClayErrors});

    Logger::logMsg(LogLabel::DEBUG, "Loading fonts");
    std::filesystem::path resourcePath = Utils::getResourcePath();
    Font fontMontserratSemiBold = LoadFont(
        (resourcePath.generic_string() + "fonts/Montserrat-SemiBold.ttf")
            .c_str());
    SetTextureFilter(fontMontserratSemiBold.texture, TEXTURE_FILTER_BILINEAR);
    Clay_SetMeasureTextFunction(Raylib_MeasureText, &fontMontserratSemiBold);

    Logger::logMsg(LogLabel::DEBUG, "Initializing configuration");
    std::shared_ptr<Configuration> configuration =
        std::make_shared<Configuration>();

    // initialize UI components 
    AppModel appModel = App_Init();
    std::queue<Message> messageQueue;

    while (!WindowShouldClose()) {
        Vector2 mousePosition = GetMousePosition();
        Vector2 scrollDelta = GetMouseWheelMoveV();
        float deltaTime = GetFrameTime();

        Clay_SetPointerState({mousePosition.x, mousePosition.y},
                             IsMouseButtonDown(MOUSE_LEFT_BUTTON));
        Clay_SetLayoutDimensions(
            {(float)GetScreenWidth(), (float)GetScreenHeight()});
        Clay_UpdateScrollContainers(
            true, (Clay_Vector2){scrollDelta.x * 8.0f, scrollDelta.y * 8.0f},
            deltaTime);

        Clay_BeginLayout();

        App_View(appModel, messageQueue);
        for (size_t i = 0; i < messageQueue.size(); i++) {
            Message msg = messageQueue.front();
            messageQueue.pop();
            appModel = App_Update(appModel, msg);
        }

        Clay_RenderCommandArray renderCommands = Clay_EndLayout();

        BeginDrawing();
        Clay_Raylib_Render(renderCommands, &fontMontserratSemiBold);
        EndDrawing();
    }

    UnloadFont(fontMontserratSemiBold);
    return 0;
}
