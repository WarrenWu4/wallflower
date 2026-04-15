# Refactor Plan - Wallflower

## Classification

| Bucket | Files |
|--------|-------|
| `core` | `src/main.cpp`, `include/clay_renderer_raylib.c`, `include/clay.h`, `include/raylib.h`, `include/raymath.h`, `include/colors.h` |
| `views` | `src/views/main_view.cpp`, `include/wallflower/views/main_view.hpp`, `src/views/action_bar.cpp`, `src/simplified_view.cpp`, `include/simplified_view.hpp`, `src/wallpaper_dropdown.cpp`, `include/wallpaper_dropdown.hpp`, `src/tabs.cpp`, `include/tabs.hpp`, `src/bouncer.cpp`, `include/bouncer.hpp` |
| `data_managers` | `src/configuration.cpp`, `include/configuration.hpp`, `src/wallpapers.cpp`, `include/wallpapers.hpp`, `src/settings.cpp`, `include/settings.hpp` |
| `utils` | `src/utils.cpp`, `include/utils.hpp`, `src/logger.cpp`, `include/logger.hpp` |

## Proposed Actions

1. **Create Folders**: `mkdir -p src/{core,views,data_managers,utils}`
2. **Move Files**:
    - `src/main.cpp` -> `src/core/main.cpp`
    - `src/configuration.cpp` -> `src/data_managers/configuration.cpp`
    - `include/configuration.hpp` -> `src/data_managers/configuration.hpp`
    - `src/wallpapers.cpp` -> `src/data_managers/wallpapers.cpp`
    - `include/wallpapers.hpp` -> `src/data_managers/wallpapers.hpp`
    - `src/utils.cpp` -> `src/utils/utils.cpp`
    - `include/utils.hpp` -> `src/utils/utils.hpp`
    - `src/logger.cpp` -> `src/utils/logger.cpp`
    - `include/logger.hpp` -> `src/utils/logger.hpp`
    - (and so on for others)
3. **Migrate Views**:
    - `MainView` (currently in `src/views/main_view.cpp`) will be refactored into:
        - `src/views/main/main.hpp`
        - `src/views/main/main_view.cpp`
        - `src/views/main/main_update.cpp`
    - Similarly for `ActionBar`, `SimplifiedView`, `WallpaperDropdown`, `Tabs`, `Bouncer`.
4. **Update Includes**: Fix all include paths after moving.
5. **Verify**: Ensure the project still builds.

**Note**: I will move headers from `include/` into their respective `src/` subfolders to keep things co-located as per the Elm architecture suggestion (e.g., `settings.h`, `settings_view.c`, `settings_update.c` in the same dir).
