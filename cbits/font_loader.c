#include <fontconfig/fontconfig.h>
#include <stdbool.h>
#include <stdio.h>

/*
 * C function to add a font file to the current FontConfig configuration.
 * This makes the font available to GTK/Pango.
 *
 * It uses a char* (const char*) for the file path, as is standard in C.
 */
bool hs_add_font_file(const char *font_file_path) {
  FcConfig *config = FcConfigGetCurrent();

  if (!config) {
    printf("Failed to get current FontConfig configuration.\n");
    return false;
  }

  return FcConfigAppFontAddFile(config, (const FcChar8 *)font_file_path);
}
