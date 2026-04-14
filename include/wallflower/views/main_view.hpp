#pragma once
#include "wallflower/views/action_bar.hpp"
#include <memory>

class MainView {
private:
  std::unique_ptr<ActionBar> actionBar = std::make_unique<ActionBar>();
public:
  MainView();
  ~MainView();
  void fetchData();
  void transformData();
  void view();
  void handleEvents();
};
