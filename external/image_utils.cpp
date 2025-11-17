bool image_test_cpp() {
  return true;
}

extern "C" {
    bool image_test() {
        return image_test_cpp();
    }
}
