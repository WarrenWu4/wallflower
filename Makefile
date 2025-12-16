CXXFLAGS = -Wall -std=c++20
BUILD_DIR = build

all: main

$(BUILD_DIR):
	mkdir build

main: $(BUILD_DIR)
	g++ src/*.cpp $(CXXFLAGS) -lraylib -lGL -lm -lpthread -ldl -lrt -lX11 -Llib -Iinclude -o build/main

run: main
	./build/main

clean:
	rm -rf build


