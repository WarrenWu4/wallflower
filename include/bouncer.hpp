#pragma once

#include <string>

class Bouncer {
public:
  Bouncer();

  static std::string executeCommand(const char *cmd);
  static bool isProcessRunning(const std::string &processName);

  static bool isHyprlandRunning();
  static bool isHyprpaperRunning();
  static bool isHyprpaperIpcEnabled();
  static bool doesZenityRun();
};
