#include "wallflower/views/action_bar.hpp"
#include "clay.h"
#include "colors.h"

ActionBar::ActionBar() {
  fetchData();
  transformData();
}

ActionBar::~ActionBar() {

}

void ActionBar::fetchData() {

}

void ActionBar::transformData() {

}

void ActionBar::view() {
  CLAY(CLAY_ID("ActionBar"), {
    .layout = {
      .sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
      .childAlignment = { .y = CLAY_ALIGN_Y_CENTER }
    }
  }) {
    CLAY(CLAY_ID("Test"), {
        .layout = {
          .sizing = {CLAY_SIZING_FIXED(20), CLAY_SIZING_FIXED(20)},
        },
        .backgroundColor = COLOR_ORANGE_LIGHT,
    });
  }
}

void ActionBar::handleEvents() {

}
