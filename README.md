# wallflower

GUI interface for hyprpaper.

## Installation

Requires docker to be setup.

### Linux (Wayland)

```bash
docker run -it \
    --env="WAYLAND_DISPLAY" \
    --env="XDG_RUNTIME_DIR" \
    --volume="${XDG_RUNTIME_DIR}/${WAYLAND_DISPLAY}:/tmp/${WAYLAND_DISPLAY}" \
    --volume="${XDG_RUNTIME_DIR}/pulse:/tmp/pulse" \
    --device=/dev/dri \
    <image_name> \
    <start_gui>
```

## Usage

- by default will find images from ~/background
- can add and change multiple directories for finding images
- runs a script through hyprpaper to change the background

## Configuration 
