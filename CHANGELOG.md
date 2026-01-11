# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

Dates are in the form: **mm-dd-yyyy**

---

## [Unreleased]

### Added
- Updated README.md to include installation through Arch AUR and yay

## [0.0.2] - 01-10-2026

### Added

- Makefile builds with dependency (.d) files for faster compilation.
- Added file logging in production under "~/.local/state/wallflower/wallflower.log" for production debugging.
- Updated README.md to include contributing section
- MIT LICENSE added to project and installed in PKGBUILD

### Changed

- PKGBUILD now uses install commands for best practice

### Fixed

- writeConfigToFile had 2 opening braces in config template which is now fixed
- hyprpaper.conf was not updating due to using local class variable. Lot of changes to structure of code which fixes this issue and is handled by configuration class
- Save file does not update due to missing root permission. Fixed by changing location in production code

## [0.0.1] - 01-04-2026 

### Added

- Created scroll box for image viewing
- Added buttons for configuring image fit
- Created settings screen to change which directories to search
- On click event for images runs hyprctl IPC to update active wallpaper
- Created parser for hyprpaper.conf for syncing updates to wallpaper

---

[0.0.1]: https://github.com/WarrenWu4/wallflower/releases/tag/v0.0.1
