#include "tabs.hpp"

Tabs::Tabs(TabType initType, std::shared_ptr<Wallpapers> wp, std::shared_ptr<Settings> settingsPtr) {
  currentTab = initType;
  this->wp = wp;
  this->settingsPtr = settingsPtr;
  settingsIcon = LoadTexture("resources/icons/folder-icon.png");
  closeIcon = LoadTexture("resources/icons/folder-icon.png");
}

Tabs::~Tabs() {
  UnloadTexture(settingsIcon);
  UnloadTexture(closeIcon);
}

void Tabs::tabEl() {
  CLAY(CLAY_ID("TabsContainer"), {
    .layout = {
      .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_FIT()},
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
        .padding = {12, 12, 12, 12}
      },
      .backgroundColor = COLOR_BLUE_DARK,
      .cornerRadius = {28, 28, 28, 28},
    }) {
      CLAY(CLAY_ID("TabsButtonIcon"), {
        .layout = { 
          .sizing = { .width = CLAY_SIZING_FIXED(16), .height = CLAY_SIZING_FIXED(16) }
        }, 
        .image = { .imageData = &settingsIcon} 
      }) {}
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
      }
    }
  }
}
