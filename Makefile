CXX = g++
CXXFLAGS = -Wall -std=c++20
LIBFLAGS = -lraylib -lGL -lm -lpthread -ldl -lrt -lX11 -Llib -Iinclude
PRODFLAGS = -O3 -DBUILD_MODE_PROD

BUILD_DIR = build
DEV_DIR = $(BUILD_DIR)/dev
PROD_DIR = $(BUILD_DIR)/prod
RESOURCE_DIR = resources

SRC = $(wildcard src/*.cpp)
HEADERS = $(wildcard include/*.h*)
DEV_OBJ = $(patsubst src/%.cpp, $(DEV_DIR)/%.o, $(SRC))
PROD_OBJ = $(patsubst src/%.cpp, $(PROD_DIR)/%.o, $(SRC))

EXE = main
DEV_EXE = $(DEV_DIR)/$(EXE)
PROD_EXE = $(PROD_DIR)/$(EXE)

all: dev 

dev: $(DEV_OBJ) 
	$(CXX) $(DEV_OBJ) $(CXXFLAGS) $(LIBFLAGS) -o $(DEV_EXE) 

prod: $(PROD_OBJ)
	$(CXX) $(PROD_OBJ) $(CXXFLAGS) $(LIBFLAGS) -O3 -DBUILD_MODE_PROD -o $(PROD_EXE) 

run: dev
	./$(DEV_EXE)

runp: prod
	./$(PROD_EXE)

clean:
	rm -rf $(BUILD_DIR) 

$(DEV_DIR)/%.o: src/%.cpp $(HEADERS) | $(DEV_DIR)
	$(CXX) $(CXXFLAGS) $(LIBFLAGS) -c $< -o $@

$(DEV_DIR):
	mkdir -p $(DEV_DIR) 

$(PROD_DIR)/%.o: src/%.cpp $(HEADERS) | $(PROD_DIR)
	$(CXX) $(CXXFLAGS) $(LIBFLAGS) $(PRODFLAGS) -c $< -o $@

$(PROD_DIR):
	mkdir -p $(PROD_DIR)

.PHONY: all dev prod run runp clean
