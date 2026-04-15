---
name: raylib-clay-refactor
description: >
  Refactor a raylib + clay desktop application codebase. Use this skill whenever
  the user wants to reorganize a raylib/clay C or C++ project into structured
  folders (views, data_managers, utils, core), migrate views to the Elm
  architecture (Model / Update / View), or do any combination of structural
  cleanup in a clay-based UI project. Trigger this skill whenever the user
  mentions raylib, clay layout manager, Elm architecture in C/C++, or asks to
  reorganize a desktop application codebase.
---

# Raylib + Clay Refactor Agent

Restructure a raylib + clay desktop app to:
1. Organize code into `core/`, `views/`, `data_managers/`, `utils/`
2. Make every view follow Elm architecture: **Model → update() → view()**

**Guiding principle:** prefer the simplest code that communicates intent clearly.
Fewer lines, obvious names, no clever abstractions.

---

## Step 0 — Read First

List and read every source file before touching anything:

```bash
find . -name "*.c" -o -name "*.h" -o -name "*.cpp" -o -name "*.hpp"
```

Classify each file into one bucket:

| Bucket | Signal |
|--------|--------|
| `core` | has `main()`, `InitWindow`, or the game loop |
| `views` | has `CLAY(` or raylib draw calls |
| `data_managers` | owns file I/O, parsing, caching, or app-wide state |
| `utils` | pure functions, no global state, no draw calls |

Write the classification to `refactor_plan.md`. Confirm with the user before moving anything.

---

## Step 1 — Create Folders

```bash
mkdir -p src/{core,views,data_managers,utils}
```

Leave `assets/`, `vendor/`, `lib/` alone.

---

## Step 2 — Move Non-View Files

Move `core`, `utils`, and `data_managers` files first — they need no
structural changes, just a new home.

After each move: update `#include` paths, then verify the build compiles.

---

## Step 3 — Migrate Each View to Elm Architecture

One view at a time. Each view gets **one header** and **two `.c` files**:

```
views/settings/
    settings.h        ← Model struct + Msg enum + function declarations
    settings_view.c   ← Reads model, emits clay, returns a Msg
    settings_update.c ← Takes a Msg, returns updated Model
```

### The header

Everything a caller needs to know, nothing more:

```c
// views/settings/settings.h
#pragma once
#include <stdbool.h>

typedef enum {
    SETTINGS_MSG_NONE,
    SETTINGS_MSG_TOGGLE_DARK_MODE,
    SETTINGS_MSG_SAVE,
} SettingsMsg;

typedef struct {
    bool  darkMode;
    float volume;   // 0.0 – 1.0
} SettingsModel;

SettingsModel   Settings_Update(SettingsModel m, SettingsMsg msg);
SettingsMsg     Settings_View(const SettingsModel *m);
```

Keep the Model flat. If a field needs explanation, a short inline comment is enough.
Skip constructors/destructors unless the model allocates heap memory.

### view() — read model, return a message

```c
// views/settings/settings_view.c
#include "settings.h"
#include "clay.h"
#include "raylib.h"

SettingsMsg Settings_View(const SettingsModel *m) {
    CLAY(CLAY_ID("Settings"), CLAY_LAYOUT({...})) {

        CLAY(CLAY_ID("DarkModeToggle"), CLAY_LAYOUT({...})) {
            // show current state
            if (Clay_Hovered() && IsMouseButtonPressed(MOUSE_LEFT_BUTTON))
                return SETTINGS_MSG_TOGGLE_DARK_MODE;
        }

        CLAY(CLAY_ID("SaveButton"), CLAY_LAYOUT({...})) {
            if (Clay_Hovered() && IsMouseButtonPressed(MOUSE_LEFT_BUTTON))
                return SETTINGS_MSG_SAVE;
        }
    }

    return SETTINGS_MSG_NONE;
}
```

Rules: read `m`, never write it. Return the first message that fires; ignore the rest.

### update() — take a message, return new model

```c
// views/settings/settings_update.c
#include "settings.h"
#include "../../data_managers/app_settings.h"

SettingsModel Settings_Update(SettingsModel m, SettingsMsg msg) {
    switch (msg) {
        case SETTINGS_MSG_TOGGLE_DARK_MODE: m.darkMode = !m.darkMode; break;
        case SETTINGS_MSG_SAVE:             AppSettings_Save(m);       break;
        default: break;
    }
    return m;
}
```

Rules: no clay calls, no draw calls. Return the model by value.

### Wire into the main loop

```c
SettingsModel settings = {0};   // zero-init is a valid default

while (!WindowShouldClose()) {
    SettingsMsg msg = Settings_View(&settings);
    settings        = Settings_Update(settings, msg);

    Clay_RenderCommandArray cmds = Clay_EndLayout();
    RaylibRender(cmds);
}
```

---

## Step 4 — Verify

- [ ] Compiles with no new warnings
- [ ] `update()` has zero `CLAY(` calls
- [ ] `view()` never writes through `m`
- [ ] `InitWindow` / `CloseWindow` only appear in `core/`
- [ ] `utils/` has no global mutable state

---

## When Things Are More Complex

**Messages with data** (e.g. a slider value): add a payload field only if needed.

```c
typedef struct {
    SettingsMsg tag;
    float       volume;   // only valid when tag == SETTINGS_MSG_SET_VOLUME
} SettingsMsgData;
```

**Multiple screens**: hold a screen tag + union of models in `core/`:

```c
typedef struct {
    enum { SCREEN_HOME, SCREEN_SETTINGS } active;
    union { HomeModel home; SettingsModel settings; };
} AppModel;
```

**Shared components** (buttons, modals): give them their own `Model`/`Msg`/`view`/`update` in `views/components/`.

**Async loading**: emit a `REQUEST_LOAD` message → `update()` calls `DataManager_BeginLoad()`, sets `loading = true` → poll next frame.

---

## Working Principles

- **Read before writing.** Classify every file before moving any.
- **One move at a time.** Verify the build after each change.
- **Simplest code that works.** Resist adding layers until complexity demands them.
- **Log moves** in `refactor_plan.md` so nothing is silently lost.
