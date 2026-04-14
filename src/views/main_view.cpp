#include "wallflower/views/main_view.hpp"
#include "clay.h"
#include "colors.h"

MainView::MainView() {

}

MainView::~MainView() {

}

void MainView::fetchData() {

}

void MainView::transformData() {

}

void MainView::view() {
  CLAY(CLAY_ID("MainContainer"), {
      .layout = {
        .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
        .padding = {CLAY_PADDING_ALL(16)},
        .childGap = 16,
        .layoutDirection = CLAY_TOP_TO_BOTTOM
      },
      .backgroundColor = COLOR_BACKGROUND_1,
  }) {
    this->actionBar->view();
  }
}

void MainView::handleEvents() {

}
