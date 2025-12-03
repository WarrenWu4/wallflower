#include <cstdint>
#include <memory>

struct Size {
  int width;
  int height;
};

struct Pixel {
  uint8_t r;
  uint8_t g;
  uint8_t b;
  uint8_t a;
};

class Png {
public:
  Size getImageDimension(const char *filePath) { return {0, 0}; }
  Pixel* getImagePixels(const char *filePath) {
    std::unique_ptr<Pixel> pixels = std::make_unique<Pixel>();
    return pixels.release();
  }
};

class Jpg {
public:
  Size getImageDimension(const char *filePath) { return {0, 0}; }
  Pixel* getImagePixels(const char *filePath) {
    std::unique_ptr<Pixel> pixels = std::make_unique<Pixel>();
    return pixels.release();
  }
};

extern "C" {
Size get_png_dimension(const char *filePath) {
  Png png;
  return png.getImageDimension(filePath);
}
Size get_jpg_dimension(const char *filePath) {
  Jpg jpg;
  return jpg.getImageDimension(filePath);
}
}
