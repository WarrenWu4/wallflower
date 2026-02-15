#include "tabs.hpp"
#include "settings.hpp"
#include "simplified_view.hpp"
#include "utils.hpp"
#include "logger.hpp"
#include "colors.h"

void Tabs::loadMenuButtonData(std::filesystem::path resourcePath) {
  // TODO: add loading of menu button icon
  menuButtonData.showDropdown = false;
}

void Tabs::menuButtonEl() {
  CLAY(CLAY_ID("MenuButton"), {
    .layout = {
      .sizing = {CLAY_SIZING_FIXED(16), CLAY_SIZING_FIXED(16)},
    },
    .backgroundColor = COLOR_BLUE_LIGHT,
  }) {

    if (Clay_Hovered()) {
      SetMouseCursor(MOUSE_CURSOR_POINTING_HAND);
      if (IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
        menuButtonData.showDropdown = !menuButtonData.showDropdown;
      }
    } else {
      SetMouseCursor(MOUSE_CURSOR_DEFAULT);
    }

    if (menuButtonData.showDropdown) {

      CLAY(CLAY_ID("MenuButtonDropdown"), {
        .layout = {
          .sizing = { CLAY_SIZING_FIT(), CLAY_SIZING_FIT() },
          .padding = {12, 12, 12, 12},
          .childGap = 8,
          .layoutDirection = CLAY_TOP_TO_BOTTOM
        },
        .backgroundColor = COLOR_BACKGROUND_2,
        .floating = {
          .offset = { 0, 8 },
          .attachPoints = {
            .element = CLAY_ATTACH_POINT_RIGHT_TOP,
            .parent = CLAY_ATTACH_POINT_RIGHT_BOTTOM
          },
          .attachTo = CLAY_ATTACH_TO_PARENT,
        }
      }) {
        for (size_t i = 0; i < tabData.size(); i++) {
          CLAY(CLAY_IDI("MenuButtonDropdownItem", i), {
            .layout = {
              .sizing = { CLAY_SIZING_FIT(), CLAY_SIZING_FIT() },
            }
          }) {
            CLAY_TEXT(
              Clay_String({
                .length = static_cast<int32_t>(tabData.at(i).size()),
                .chars = tabData.at(i).c_str()
              }),
              CLAY_TEXT_CONFIG({
                .textColor = COLOR_FOREGROUND_3,
                .fontSize = 20
              })
            );
          }
        }
      }

    }
  }
}

Tabs::Tabs(TabType initType, std::shared_ptr<Wallpapers> wp, std::shared_ptr<Settings> settingsPtr, std::shared_ptr<SimplifiedView> simplifiedPtr) {
  currentTab = initType;
  this->wp = wp;
  this->settingsPtr = settingsPtr;
  this->simplifiedPtr = simplifiedPtr;
  std::filesystem::path resourcePath = Utils::getResourcePath();
  settingsIcon = LoadTexture((resourcePath.generic_string() + "icons/settings-icon.png").c_str());
  galleryIcon = LoadTexture((resourcePath.generic_string() + "icons/wallpaper-icon.png").c_str());
  loadMenuButtonData(resourcePath);
}

Tabs::~Tabs() {
  UnloadTexture(settingsIcon);
  UnloadTexture(galleryIcon);
  Logger::logMsg(LogLabel::OK, "Destructor ran");
}

void Tabs::tabEl() {
  CLAY(CLAY_ID("TabsContainer"), {
    .layout = {
      .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_FIT()},
      .childAlignment = { .y = CLAY_ALIGN_Y_CENTER }
    }
  }) {
    CLAY_TEXT(
        Clay_String({
          .length = static_cast<int32_t>(tabData.at(static_cast<int>(currentTab)).size()),
          .chars = tabData.at(static_cast<int>(currentTab)).c_str()
        }),
        CLAY_TEXT_CONFIG({ 
          .textColor = COLOR_FOREGROUND_1,
          .fontSize = 32 
        })
    );
    CLAY(CLAY_ID("TabsSpacer"), {
      .layout = {
        .sizing = {CLAY_SIZING_GROW(), CLAY_SIZING_GROW()},
      }
    }) {}
    CLAY(CLAY_ID("TabsButton"), {
      .layout = {
        .sizing = {CLAY_SIZING_FIT(), CLAY_SIZING_FIT()},
        .padding = {8, 8, 8, 8}
      },
      .backgroundColor = (Clay_Hovered()) ? COLOR_FOREGROUND_1 : COLOR_FOREGROUND_3,
      .cornerRadius = {24, 24, 24, 24},
    }) {
      if (Clay_Hovered()) {
        SetMouseCursor(MOUSE_CURSOR_POINTING_HAND);
        if (IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
          currentTab = (currentTab == TabType::Gallery) ? TabType::Settings : TabType::Gallery;
        }
      } else {
        SetMouseCursor(MOUSE_CURSOR_DEFAULT);
      }
      CLAY(CLAY_ID("TabsButtonIcon"), {
        .layout = { 
          .sizing = { .width = CLAY_SIZING_FIXED(16), .height = CLAY_SIZING_FIXED(16) }
        }, 
        .image = { .imageData = (currentTab == TabType::Gallery) ? &settingsIcon : &galleryIcon} 
      }) {}
      menuButtonEl();
    }
  }
}

void Tabs::bodyEl() {
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
      switch (currentTab) {
        case TabType::Gallery:
          wp->wallpaperContainerEl();
          break;
        case TabType::Settings:
          settingsPtr->settingsContainerEl();
          break;
        case TabType::Simplified:
          simplifiedPtr->SimplifiedViewEl();
          break;
      }
      Clay_ScrollContainerData scrollData = Clay_GetScrollContainerData(CLAY_ID("ScrollContainer"));
      Clay_Vector2 scrollOffset = Clay_GetScrollOffset();

      if (scrollData.contentDimensions.height > scrollData.scrollContainerDimensions.height) {
        float trackHeight = scrollData.scrollContainerDimensions.height;
        float contentHeight = scrollData.contentDimensions.height;
        float thumbHeight = (trackHeight / contentHeight) * trackHeight;
        float thumbY = ((-scrollOffset.y) / contentHeight) * trackHeight;

        CLAY(CLAY_ID("ScrollBar"), {
            .floating = {
              .offset = { 0, thumbY },
              .attachPoints = { .element = CLAY_ATTACH_POINT_RIGHT_TOP, .parent = CLAY_ATTACH_POINT_RIGHT_TOP },
              .attachTo = CLAY_ATTACH_TO_PARENT,
            }
        }) {
          CLAY(CLAY_ID("ScrollThumb"), {
              .layout = {
                .sizing = { .width = CLAY_SIZING_FIXED(6), .height = CLAY_SIZING_FIXED(thumbHeight) }
              },
              .backgroundColor = COLOR_FOREGROUND_4, 
          }) {}
        }
      }

    }
  }
}
