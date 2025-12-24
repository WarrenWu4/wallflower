#pragma once

#include <string>
#include <vector>

class Bouncer {
public:
  static inline const std::vector<std::string_view> supportedHyprpaperVersions = {
    "0.7.6-4"
  };
  static std::string currentVersion;

  Bouncer();

  static std::string executeCommand(const char *cmd);
  static bool isProcessRunning(const std::string &processName);
  static std::vector<std::string> split(const std::string& str, char delimiter);

  static std::string getHyprpaperVersion();

  static bool isHyprlandRunning();
  static bool isHyprpaperRunning();
  static bool isHyprpaperIpcEnabled();
  static bool isHyprpaperVersionSupported();
  static bool doesZenityRun();
};
