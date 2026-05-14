#pragma once
#include "core/messages.hpp"
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
SearchBarModel SearchBar_Init();
SearchBarModel SearchBar_Update(SearchBarModel model,
                                SearchBarMessageGroup message);
void SearchBar_View(SearchBarModel m, std::queue<Message> &messageQueue);
