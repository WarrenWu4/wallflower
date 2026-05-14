#pragma once
#include <string>
#include <variant>

namespace SearchBarMessages {
struct Query {
    std::string query;
};
struct Select {
    int optionIndex;
};
} // namespace SearchBarMessages

using SearchBarMessageGroup =
    std::variant<SearchBarMessages::Query, SearchBarMessages::Select>;

namespace GalleryMessages {
struct SetWallpaper {
    std::string wallpaperPath;
};
struct ToggleDropdown {};
}; // namespace GalleryMessages

using GalleryMessageGroup =
    std::variant<GalleryMessages::SetWallpaper, GalleryMessages::ToggleDropdown>;

using Message = std::variant<SearchBarMessageGroup, GalleryMessageGroup>;
