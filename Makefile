CXX = g++
CXXFLAGS = -Wall -std=c++20
LIBFLAGS = -lraylib -lGL -lm -lpthread -ldl -lrt -lX11 -Llib -Iinclude
BUILD_DIR = build
SRC = $(wildcard src/*.cpp)
HEADERS = $(wildcard include/*.h*)
OBJ = $(patsubst src/%.cpp, $(BUILD_DIR)/%.o, $(SRC))
RESOURCE_DIR = resources
EXE = main

all: $(EXE) 

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR) 

$(BUILD_DIR)/%.o: src/%.cpp $(HEADERS) | $(BUILD_DIR)
	$(CXX) $(CXXFLAGS) $(LIBFLAGS) -c $< -o $@

$(EXE): $(OBJ)
	$(CXX) $(OBJ) $(CXXFLAGS) $(LIBFLAGS) -o $(BUILD_DIR)/$(EXE)

run: $(EXE) 
	./$(BUILD_DIR)/$(EXE)

clean:
	rm -rf $(BUILD_DIR) 

.PHONY: all run clean
