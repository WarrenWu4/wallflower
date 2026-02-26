#include "simplified_view.hpp"
#include "clay.h"
#include "colors.h"
#include "configuration.hpp"
#include "utils.hpp"
#include "logger.hpp"

void SimplifiedView::loadWallpaperListItemData() {
  const std::vector<WallpaperData> &temp = configuration->getConfig().wallpapers;
  Logger::logMsg(LogLabel::DEBUG, "Loading " + std::to_string(temp.size()) + " wallpapers for simplified view");
  for (size_t i = 0; i < temp.size(); i++) {
    const WallpaperData& data = temp.at(i);
    Texture2D preview = LoadTexture(data.path.c_str());
    wallpaperListItemData.push_back((WallpaperListItemData){
      .preview = preview,
      .name = Utils::split(data.path, '/').back(),
      .path = data.path
    });
  }
}

void SimplifiedView::unloadWallpaperListItemData() {
  for (const WallpaperListItemData& data : wallpaperListItemData) {
    UnloadTexture(data.preview);
  }
}

void SimplifiedView::WallpaperListEl() {
  CLAY(CLAY_ID("SimplifiedViewWallpaperSelector"), {
    .layout = {
      .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
      .padding = {16, 16, 16, 16},
      .childGap = 16,
      .layoutDirection = CLAY_TOP_TO_BOTTOM
    },
    .backgroundColor = COLOR_BACKGROUND_2,
    .cornerRadius = {8, 8, 8, 8},
    .clip = { .vertical = true, .childOffset = Clay_GetScrollOffset() }
  }) {
    for (size_t i = 0; i < wallpaperListItemData.size(); i++) {
        const auto& data = wallpaperListItemData.at(i);
        CLAY(CLAY_IDI("SimplifiedViewWallpaperItem", i), {
          .layout = {
            .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_FIT()},
            .padding = {8, 8, 8, 8},
            .childGap = 16,
            .childAlignment = { .y = CLAY_ALIGN_Y_CENTER }
          },
          .backgroundColor = Clay_Hovered() ? COLOR_BACKGROUND_3 : COLOR_BACKGROUND_1,
          .cornerRadius = {4, 4, 4, 4}
        }) {
          if (Clay_Hovered()) {
              SetMouseCursor(MOUSE_CURSOR_POINTING_HAND);
          }
          CLAY(CLAY_IDI("SimplifiedViewWallpaperItemPreview", i), {
            .layout = {
              .sizing = { .width = CLAY_SIZING_FIXED(64), .height = CLAY_SIZING_FIXED(36) }
            },
            .image = { .imageData = (void *)&data.preview }
          }) {}

          CLAY_TEXT(
              Clay_String({
                .length = static_cast<int32_t>(data.name.size()),
                .chars = data.name.c_str()
              }),
              CLAY_TEXT_CONFIG({
                .textColor = COLOR_FOREGROUND_0,
                .fontSize = 20
              })
          );
        }
    }
    Clay_ScrollContainerData scrollData = Clay_GetScrollContainerData(CLAY_ID("SimplifiedViewWallpaperSelector"));
    Clay_Vector2 scrollOffset = Clay_GetScrollOffset();

    if (scrollData.contentDimensions.height > scrollData.scrollContainerDimensions.height) {
        float trackHeight = scrollData.scrollContainerDimensions.height;
        float contentHeight = scrollData.contentDimensions.height;
        float thumbHeight = (trackHeight / contentHeight) * trackHeight;
        float thumbY = ((-scrollOffset.y) / contentHeight) * trackHeight;

        CLAY(CLAY_ID("SimplifiedScrollBar"), {
            .floating = {
                .offset = { -8, thumbY },
                .attachPoints = { .element = CLAY_ATTACH_POINT_RIGHT_TOP, .parent = CLAY_ATTACH_POINT_RIGHT_TOP },
                .attachTo = CLAY_ATTACH_TO_PARENT,
            }
        }) {
            CLAY(CLAY_ID("SimplifiedScrollThumb"), {
                .layout = {
                    .sizing = { .width = CLAY_SIZING_FIXED(6), .height = CLAY_SIZING_FIXED(thumbHeight) }
                },
                .backgroundColor = COLOR_FOREGROUND_4,
                .cornerRadius = {3, 3, 3, 3}
            }) {}
        }
    }
  }
}

SimplifiedView::SimplifiedView(std::shared_ptr<Configuration> configuration) : configuration(configuration) {
  loadWallpaperListItemData();
}

SimplifiedView::~SimplifiedView() {
  unloadWallpaperListItemData();
}

void SimplifiedView::SimplifiedViewEl() {
  CLAY(CLAY_ID("SimplifiedViewContainer"), {
    .layout = {
      .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
      .padding = {CLAY_PADDING_ALL(16)},
      .childGap = 8,
      .layoutDirection = CLAY_LEFT_TO_RIGHT 
    }
  }) {
    WallpaperListEl();
  }
}
