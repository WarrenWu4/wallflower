// perform these checks to ensure program can run smoothly
// just like a bouncer at the club O-O

#include "bouncer.hpp"
#include "logger.hpp"
#include <algorithm>
#include <filesystem>
#include <fstream>
#include <memory>
#include <string>

/*
 * Dependencies: (g++ compiler, Gtk, Hyprland, Hyprpaper)
 * Dependencies checked during pkg installation/build not in bouncer
 *
 * Required elements:
 * Hyprland is running
 * Hyprpaper is running
 * Hyprpaper IPC is enabled
 * zenity can run
 */

Bouncer::Bouncer() {
  // if (!(isHyprlandRunning() && isHyprpaperRunning() &&
  //       isHyprpaperIpcEnabled() && doesZenityRun())) {
  //   exit(1);
  // }
}

std::string Bouncer::executeCommand(const char *cmd) {
  char buffer[128];
  std::string result = "";
  std::unique_ptr<FILE, decltype(&pclose)> pipe(popen(cmd, "r"), pclose);
  if (!pipe)
    return "Error";
  while (fgets(buffer, sizeof(buffer), pipe.get()) != nullptr) {
    result += buffer;
  }
  // Remove trailing newline
  if (!result.empty() && result.back() == '\n')
    result.pop_back();
  return result;
}

bool Bouncer::isProcessRunning(const std::string &processName) {
  for (const auto &entry : std::filesystem::directory_iterator("/proc")) {
    if (!entry.is_directory()) {
      continue;
    }
    std::string pid = entry.path().filename().string();
    if (!std::all_of(pid.begin(), pid.end(), ::isdigit)) {
      continue;
    }
    std::ifstream cmdline(entry.path() / "comm");
    std::string line;
    if (std::getline(cmdline, line)) {
      if (line.find(processName) != std::string::npos)
        return true;
    }
  }
  return false;
}

bool Bouncer::isHyprlandRunning() {
  if (isProcessRunning("hyprland")) {
    Logger::logMsg(LogLabel::OK, "Hyprland process found.");
    return true;
  }
  Logger::logMsg(
      LogLabel::ERROR,
      "No Hyprland process found. Unable to start wallflower program.");
  return false;
}

bool Bouncer::isHyprpaperRunning() {
  if (isProcessRunning("hyprpaper")) {
    Logger::logMsg(LogLabel::OK, "Hyprpaper process found.");
    return true;
  }
  Logger::logMsg(
      LogLabel::ERROR,
      "No Hyprpaper process found. Unable to start wallflower program.");
  return false;
}

bool Bouncer::isHyprpaperIpcEnabled() {
  const char *signature = std::getenv("HYPRLAND_INSTANCE_SIGNATURE");
  if (!signature) {
    Logger::logMsg(
        LogLabel::ERROR,
        "Hyprland session not detected. Unable to start wallflower program.");
    return false;
  }
  const char* xdgRuntimeDir = std::getenv("XDG_RUNTIME_DIR");
  if (!xdgRuntimeDir) {
    Logger::logMsg(
        LogLabel::ERROR,
        "XDG_RUNTIME_DIR environment variable not found. Unable to start wallflower program.");
    return false;
  }
  std::string socketPath =
      std::string(xdgRuntimeDir) + "/hypr/" + std::string(signature) + "/.socket.sock";
  if (!std::filesystem::exists(socketPath)) {
    Logger::logMsg(
        LogLabel::ERROR,
        "Hyprpaper IPC not enabled. Unable to start wallflower program.");
    return false;
  }
  Logger::logMsg(LogLabel::OK, "Hyprpaper IPC enabled.");
  return true;
}

bool Bouncer::doesZenityRun() {
  std::string res = executeCommand("which zenity");
  if (res != "Error" && res != "") {
    Logger::logMsg(LogLabel::OK, "Zenity program found.");
    return true;
  }
  Logger::logMsg(LogLabel::ERROR, "Zenity file dialog module does not exist. "
                                  "Unable to start wallflower program.");
  return false;
}
