#pragma once
#include "core/messages.hpp"
#include <memory>
#include <queue>
#include <string>
#include <vector>

// define the app data
struct SearchBarModel {
    std::string query;
    std::string placeholder;
    std::vector<std::string> searchResults;
    std::vector<std::string> searchOptions;
    int activeOption;
};

// view functions (update to handle events and view to render UI)
std::shared_ptr<SearchBarModel> SearchBar_Init();
void SearchBar_Update(std::shared_ptr<SearchBarModel> model,
                      SearchBarMessageGroup message);
void SearchBar_View(std::shared_ptr<SearchBarModel> model,
                    std::queue<Message> &messageQueue);
