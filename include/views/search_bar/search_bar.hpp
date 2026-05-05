#pragma once
#include <string>
#include <vector>

// event/message definitions
typedef enum {
    SEARCH_BAR_MSG_QUERY,
    SEARCH_BAR_MSG_SELECT,
} SearchBarMsg;

// define the app data
typedef struct {
    std::string query;
    std::string placeholder;
    std::vector<std::string> searchResults;
    std::vector<std::string> searchOptions;
    int activeOption;
} SearchBarModel;

// view functions (update to handle events and view to render UI)
SearchBarModel SearchBar_Init();
SearchBarModel SearchBar_Update(SearchBarModel m, SearchBarMsg msg,
                                std::string payload = "");
SearchBarModel SearchBar_View(SearchBarModel m);
