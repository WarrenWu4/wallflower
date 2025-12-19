#include "dropdown.hpp"

Dropdown::Dropdown() {
  active = "";
  show = false;
  parentName = "";
  parentId = -1;
  data = nullptr;
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
      .childGap = 8,
      .layoutDirection = CLAY_TOP_TO_BOTTOM
    },
    .backgroundColor = COLOR_BACKGROUND_0,
    .floating = {
      .parentId = id,
      .attachTo = CLAY_ATTACH_TO_ELEMENT_WITH_ID
    }
  }) {
    int id = 0;
    for (auto it = items.begin(); it != items.end(); it++) {
      if ((*it).first == active) { continue; }
      dropdownItemEl(id, (*it).first, (*it).second);
      id++;
    }
  }
}

void Dropdown::dropdownItemEl(int id, const std::string& name, const std::function<void(void*)>& callback) {
  CLAY(CLAY_IDI("DropdownItem", id), {
    .layout = {
      .sizing = { .width = CLAY_SIZING_GROW(), .height = CLAY_SIZING_GROW() }
    }
  }) {
    if (Clay_Hovered() && IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
      callback(data);
    }
    CLAY_TEXT(
      Clay_String({
        .length = static_cast<int>(name.size()),
        .chars = name.c_str()
      }),
      CLAY_TEXT_CONFIG({
        .textColor = COLOR_FOREGROUND_1,
        .fontSize = 20
      })
    );
  }
}
