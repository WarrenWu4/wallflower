#include "simplified_view.hpp"
#include "clay.h"
#include "colors.h"

void SimplifiedView::WallpaperSelectorEl() {
  // create scroll container
  // create element for each wallpaper
  CLAY(CLAY_ID("SimplifiedViewWallpaperSelector"), {
    .layout = {
      .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
      .layoutDirection = CLAY_TOP_TO_BOTTOM 
    },
    .backgroundColor = COLOR_GREEN_DARK,
  }) {}
}

void SimplifiedView::WallpaperDisplayEl() {
  // display the image here 
  CLAY(CLAY_ID("SimplifiedViewContainer"), {
    .layout = {
      .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
      .layoutDirection = CLAY_LEFT_TO_RIGHT 
    },
    .backgroundColor = COLOR_GREEN_LIGHT,
  }) {}
}

SimplifiedView::SimplifiedView() {
}

SimplifiedView::~SimplifiedView() {
}

void SimplifiedView::SimplifiedViewEl() {
  CLAY(CLAY_ID("SimplifiedViewContainer"), {
    .layout = {
      .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
      .padding = {CLAY_PADDING_ALL(16)},
      .childGap = 8,
      .layoutDirection = CLAY_LEFT_TO_RIGHT 
    }
  }) {}
}
