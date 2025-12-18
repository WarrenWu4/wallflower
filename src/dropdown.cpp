#include "dropdown.hpp"
#include "raylib.h"

Dropdown::Dropdown() {
  active = "";
  show = false;
  parentName = "";
  parentId = -1;
}

void Dropdown::dropdownEl() {
  if (!show || parentName == "") { return; }
  uint32_t id;
  if (parentId != -1) {
    id = CLAY_SIDI(Clay_String({
      .length = static_cast<int32_t>(parentName.size()),
      .chars = parentName.c_str()
    }), parentId).id;
  } else {
    id = CLAY_SID(Clay_String({
      .length = static_cast<int32_t>(parentName.size()),
      .chars = parentName.c_str()
    })).id;
  }
  CLAY(CLAY_ID("DropdownContainer"), {
    .layout {
      .sizing = { .width = CLAY_SIZING_FIT(), .height = CLAY_SIZING_FIT() },
      .padding = {16, 16, 16, 16},
      .childGap = 8
    },
    .backgroundColor = COLOR_BACKGROUND_0,
    .floating = { .parentId = id }
  }) {
    int id = 0;
    for (const auto& [name, callback] : items) {
      if (name == active) { continue; }
      dropdownItemEl(id, name, callback);
      id++;
    }
  }
}

void Dropdown::dropdownItemEl(int id, std::string name, std::function<void()> callback) {
  CLAY(CLAY_IDI("DropdownItem", 1), {
    .layout = {
      .sizing = { .width = CLAY_SIZING_GROW(), .height = CLAY_SIZING_GROW() }
    }
  }) {
    if (Clay_Hovered() && IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
      callback();
    }
    CLAY_TEXT(
      CLAY_STRING("testing"),
      CLAY_TEXT_CONFIG({
        .textColor = COLOR_FOREGROUND_1,
        .fontSize = 20
      })
    );
  }
}
