#include "settings.hpp"
#include "colors.h"
#include "logger.hpp"
#include "utils.hpp"

Settings::Settings(std::shared_ptr<Configuration> configuration) {
  searchPathToRemove = "";
  this->configuration = configuration;
  std::filesystem::path resourcePath = Utils::getResourcePath();
  folderIcon = LoadTexture(
      (resourcePath.generic_string() + "icons/folder-icon.png").c_str());
  fileIcon = LoadTexture(
      (resourcePath.generic_string() + "icons/file-icon.png").c_str());
  defaultMode = FitMode::COVER;
}

Settings::~Settings() {
  UnloadTexture(folderIcon);
  UnloadTexture(fileIcon);
  Logger::logMsg(LogLabel::OK, "Destructor ran");
}

void Settings::folderPicker() {
  std::string cmd = "zenity --file-selection --directory --multiple "
                    "--separator=\"|\" --title=\"Add Folder\"";
  FILE *fp = popen(cmd.c_str(), "r");
  if (fp) {
    // FIX: hard coding max path length could be an issue?
    // if the user has a really long path or is selecting a bunch of paths
    // addDirectories() does validates paths so there shouldn't be a weird
    // issue of cutoff/partial path being added
    char path[1024];
    if (fgets(path, sizeof(path), fp) != NULL) {
      std::string s(path);
      s.erase(s.find_last_not_of("\n\r") + 1);
      std::vector<std::string> folders = Utils::split(s, '|');
      configuration->addDirectories(folders);
    }
    pclose(fp);
  }
}

void Settings::imagePicker() {
  std::string cmd = "zenity --file-selection --multiple --separator=\"|\" "
                    "--title=\"Add Image\" --file-filter=\"Supported Images | ";
  for (const std::string &imageFormat : supportedImageFormats) {
    cmd += "*" + imageFormat + " ";
  }
  cmd += "\"";
  Logger::logMsg(LogLabel::DEBUG, cmd);
  FILE *fp = popen(cmd.c_str(), "r");
  if (fp) {
    char path[1024];
    if (fgets(path, sizeof(path), fp) != NULL) {
      std::string s(path);
      s.erase(s.find_last_not_of("\n\r") + 1);
      std::vector<std::string> images = Utils::split(s, '|');
      // TODO: I know that addDirectories is a shit name
      // that is no longer accurate since it can be any
      // search path, but too lazy to change it for now
      configuration->addDirectories(images);
    }
    pclose(fp);
  }
}

void Settings::addFolderButtonEl() {
  CLAY(CLAY_ID("AddFolderButton"),
       {
           .layout = {.sizing = {CLAY_SIZING_FIT(), CLAY_SIZING_FIT()},
                      .padding = {8, 8, 8, 8}},
           .backgroundColor =
               (Clay_Hovered()) ? COLOR_FOREGROUND_1 : COLOR_FOREGROUND_3,
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
    CLAY(CLAY_ID("AddFolderButtonIcon"),
         {.layout = {.sizing = {.width = CLAY_SIZING_FIXED(16),
                                .height = CLAY_SIZING_FIXED(16)}},
          .image = {.imageData = &folderIcon}}) {}
  }
}

void Settings::addImageButtonEl() {
  CLAY(CLAY_ID("AddImageButton"),
       {
           .layout = {.sizing = {CLAY_SIZING_FIT(), CLAY_SIZING_FIT()},
                      .padding = {8, 8, 8, 8}},
           .backgroundColor =
               (Clay_Hovered()) ? COLOR_FOREGROUND_1 : COLOR_FOREGROUND_3,
           .cornerRadius = {24, 24, 24, 24},
       }) {
    if (Clay_Hovered()) {
      SetMouseCursor(MOUSE_CURSOR_POINTING_HAND);
      if (IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
        imagePicker();
      }
    } else {
      SetMouseCursor(MOUSE_CURSOR_DEFAULT);
    }
    CLAY(CLAY_ID("AddImageButtonIcon"),
         {.layout = {.sizing = {.width = CLAY_SIZING_FIXED(16),
                                .height = CLAY_SIZING_FIXED(16)}},
          .image = {.imageData = &fileIcon}}) {}
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
      CLAY(CLAY_ID("SettingsButtons"),
           {.layout = {.sizing = {CLAY_SIZING_GROW(0), CLAY_SIZING_FIT()},
                       .childGap = 8,
                       .layoutDirection = CLAY_LEFT_TO_RIGHT}}) {
        addFolderButtonEl();
        addImageButtonEl();
      }
      int id = 0;
      // INFO: defer deletion until next render
      // cycle to avoid internal hashing issues
      // ABSOLUTELY DO NOT PUT THIS AFTER THE
      // FOR LOOP OR THERE WILL SEG FAULTS
      if (searchPathToRemove != "") {
        configuration->removeDirectories({searchPathToRemove});
        searchPathToRemove = "";
        Logger::logMsg(LogLabel::OK, "Removed search path from configuration");
      }

      for (auto it = configuration->getConfig().searchPaths.begin();
           it != configuration->getConfig().searchPaths.end(); it++, id++) {
        folderEl(id, *it, searchPathToRemove);
      }
    }
  }
}

void Settings::folderEl(int id, const std::string &searchPath,
                        std::string &searchPathToRemove) {
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
      searchPathToRemove = searchPath;
    }
    CLAY(CLAY_IDI("FolderIcon", id),
         {.layout = {.sizing = {.width = CLAY_SIZING_FIXED(16),
                                .height = CLAY_SIZING_FIXED(16)}},
          .image = {.imageData = &folderIcon}}) {}
    CLAY_TEXT(
        Clay_String({.length = static_cast<int32_t>(searchPath.size()),
                     .chars = searchPath.c_str()}),
        CLAY_TEXT_CONFIG({.textColor = COLOR_FOREGROUND_1, .fontSize = 20}));
  }
}
