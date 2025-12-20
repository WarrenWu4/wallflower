#include "settings.hpp"

Settings::Settings(std::shared_ptr<Configuration> configuration) {
  this->configuration = configuration;
  folderIcon = LoadTexture("resources/icons/folder-icon.png");
  defaultMode = WallpaperMode::COVER;
}

Settings::~Settings() { UnloadTexture(folderIcon); }

void Settings::folderPicker() {
  FILE *fp = popen("zenity --file-selection --directory", "r");
  if (fp) {
    char path[1024];
    if (fgets(path, sizeof(path), fp) != NULL) {
      std::string s(path);
      s.erase(s.find_last_not_of("\n\r") + 1);
      configuration->directories.insert(s);
    }
    pclose(fp);
  }
}

void Settings::addFolderButtonEl() {
  CLAY(CLAY_ID("AddFolderButton"), {
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
          folderPicker();
        }
      } else {
        SetMouseCursor(MOUSE_CURSOR_DEFAULT);
      }
      CLAY(CLAY_ID("AddFolderButtonIcon"), {
        .layout = { 
          .sizing = { .width = CLAY_SIZING_FIXED(16), .height = CLAY_SIZING_FIXED(16) }
        }, 
        .image = { .imageData = &folderIcon } 
      }) {}
    }
}

void Settings::settingsContainerEl() {
  CLAY(CLAY_ID("SettingsContainer"),
       {
           .layout = {.sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
                      .layoutDirection = CLAY_TOP_TO_BOTTOM},
       }) {
    CLAY(CLAY_ID("FolderContainer"),
         {
             .layout = {.sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0)},
                        .childGap = 8,
                        .layoutDirection = CLAY_TOP_TO_BOTTOM},
         }) {
          addFolderButtonEl();
      std::string dirToRemove = "";
      int id = 0;
      for (auto it = this->configuration->directories.begin();
           it != this->configuration->directories.end(); it++, id++) {
        folderEl(id, *it, dirToRemove);
      }
      if (dirToRemove != "") {
        this->configuration->directories.erase(dirToRemove);
      }
    }
  }
}

void Settings::folderEl(int id, const std::string &directory,
                        std::string &dirToRemove) {
  CLAY(CLAY_IDI("Folder", id),
       {
           .layout = {.sizing = {CLAY_SIZING_FIT(), CLAY_SIZING_FIT()},
                      .padding = {12, 12, 8, 8},
                      .childGap = 8,
                      .childAlignment = {.x = CLAY_ALIGN_X_CENTER,
                                         .y = CLAY_ALIGN_Y_CENTER}},
           .border{.color = COLOR_FOREGROUND_1, .width = {2, 2, 2, 2, 0}},
       }) {
    if (Clay_Hovered() && IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
      dirToRemove = directory;
    }
    CLAY(CLAY_IDI("FolderIcon", id),
         {.layout = {.sizing = {.width = CLAY_SIZING_FIXED(16),
                                .height = CLAY_SIZING_FIXED(16)}},
          .image = {.imageData = &folderIcon}}) {}
    CLAY_TEXT(
        Clay_String({.length = static_cast<int32_t>(directory.size()),
                     .chars = directory.c_str()}),
        CLAY_TEXT_CONFIG({.textColor = COLOR_FOREGROUND_1, .fontSize = 20}));
  }
}
