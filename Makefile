CXX = g++
CXXFLAGS = -Wall -std=c++20 -fsanitize=address
LIBFLAGS = -lraylib -lGL -lm -lpthread -ldl -lrt -lX11 -lXrandr -lwayland-client -Llib -Iinclude
PRODFLAGS = -O3 -DBUILD_MODE_PROD
DEPFLAGS = -MMD -MP

BUILD_DIR = build
DEV_DIR = $(BUILD_DIR)/dev
PROD_DIR = $(BUILD_DIR)/prod
RESOURCE_DIR = resources

SRC = $(wildcard src/*.cpp)
HEADERS = $(wildcard include/*.h*)

DEV_OBJ = $(patsubst src/%.cpp, $(DEV_DIR)/%.o, $(SRC))
PROD_OBJ = $(patsubst src/%.cpp, $(PROD_DIR)/%.o, $(SRC))

DEV_DEP = $(DEV_OBJ:.o=.d)
PROD_DEP = $(PROD_OBJ:.o=.d)

EXE = main
DEV_EXE = $(DEV_DIR)/$(EXE)
PROD_EXE = $(PROD_DIR)/$(EXE)

.PHONY: all dev prod run runp clean

all: dev 

# --- DEV TARGETS ---

dev: $(DEV_OBJ) 
	$(CXX) $(DEV_OBJ) $(CXXFLAGS) $(LIBFLAGS) -o $(DEV_EXE)

$(DEV_DIR)/%.o: src/%.cpp $(HEADERS) | $(DEV_DIR)
	$(CXX) $(CXXFLAGS) $(LIBFLAGS) $(DEPFLAGS) -c $< -o $@

# --- PROD TARGETS ---

prod: $(PROD_OBJ)
	$(CXX) $(PROD_OBJ) $(CXXFLAGS) $(LIBFLAGS) -O3 -DBUILD_MODE_PROD -o $(PROD_EXE)

$(PROD_DIR)/%.o: src/%.cpp $(HEADERS) | $(PROD_DIR)
	$(CXX) $(CXXFLAGS) $(LIBFLAGS) $(DEPFLAGS) $(PRODFLAGS) -c $< -o $@

# --- MISC ---
$(DEV_DIR) $(PROD_DIR):
	mkdir -p $@

run: dev
	./$(DEV_EXE)

runp: prod
	./$(PROD_EXE)

clean:
	rm -rf $(BUILD_DIR) 

-include $(DEV_DEP) $(PROD_DEP)

