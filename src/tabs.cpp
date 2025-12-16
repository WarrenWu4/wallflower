#include "tabs.hpp"
#include "clay.h"
#include "colors.h"
#include <iostream>

Tabs::Tabs(TabType initType, std::shared_ptr<Wallpapers> wp) {
  currentTab = initType;
  this->wp = wp;
}

void Tabs::tabEl() {
  CLAY(CLAY_ID("TabsContainer"), {
    .layout = {
      .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_FIT()},
      .childGap = 8
    }
  }) {
    for (int i = 0; i < numTabs; i++) {
      CLAY(CLAY_IDI("Tab", i), {
        .layout = {
          .sizing = { CLAY_SIZING_FIT(), CLAY_SIZING_FIT() },
          .padding = {16, 16, 8, 8},
          .childAlignment = { CLAY_ALIGN_X_CENTER, CLAY_ALIGN_Y_CENTER }
        },
        .backgroundColor = COLOR_FOREGROUND_1
      }) {
        CLAY_TEXT(
            Clay_String({
              .length = (int) tabData.at(i).size(),
              .chars = tabData.at(i).c_str()
            }),
            CLAY_TEXT_CONFIG({ 
              .textColor = COLOR_BACKGROUND_0,
              .fontSize = 20
            })
        );
      }
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
          std::cout << "WIP\n";
          break;
      }
    }
  }
}
