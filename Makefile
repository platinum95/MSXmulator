CPP		    = g++
INCLUDES    = -I$(PWD)/include
CFLAGS	    = $(INCLUDES) -std=c++20 -Wno-unknown-pragmas -MMD -O0 -g -Wall -Werror -Wextra -Wformat=2 -Wshadow -pedantic -Werror=vla -march=native -Wno-unused-variable
LIBS		=

#-fconcepts-diagnostics-depth=5

DEFINES	 =
DEFINES	:=

ROOT_DIR	= $(CURDIR)
INCLUDE_DIR = $(ROOT_DIR)/include
SRC_DIR     = $(ROOT_DIR)/src
OBJ_DIR     = $(ROOT_DIR)/obj
TESTS_DIR   = $(ROOT_DIR)/src/tests

DIRS_TO_MAKE   := $(OBJ_DIR)

SRC     = $(wildcard $(SRC_DIR)/*.cpp)
TESTS   = $(wildcard $(TESTS_DIR)/*.cpp)
TOOLS   = $(wildcard $(TOOLS_DIR)/*.cpp)

MSXMULATOR_SRC  = $(SRC_DIR)/main.cpp

OBJ         = $(patsubst $(SRC_DIR)/%.cpp,$(OBJ_DIR)/%.o,$(SRC))
TOOLS_OBJ   = $(patsubst $(TOOLS_DIR)/%.cpp,$(OBJ_DIR)/%.o,$(TOOLS))
TOOLS_BIN   = $(patsubst $(OBJ_DIR)/%.o,$(ROOT_DIR)/%,$(TOOLS_OBJ))

all: msxmulator

msxmulator: $(OBJ) $(OBJ)
	$(CPP) -o $@ $^ $(CFLAGS) $(LIBS)

tests: $(OBJ) $(TESTS_OBJ) $(TEST_OBJ) $(CMORE_STATIC_LIB) | $(SHADERS_OBJ)
	$(CPP) -o $@ $^ $(CFLAGS) $(LIBS)

make_func_list:
	$(CPP) -o $@ $^ $(CFLAGS) $(LIBS)

.PHONY: tools
tools: | $(OBJ) $(CMORE_STATIC_LIB)
	$(MAKE) -C $(TOOLS_DIR) OBJ_DIR=$(OBJ_DIR)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp
	$(CPP) -c -o $@ $< $(CFLAGS)

$(OBJ_DIR)/%.o: $(TESTS_DIR)/%.cpp
	$(CPP) -c -o $@ $< $(CFLAGS)

$(OBJ_DIR)/%.o: $(TOOLS_DIR)/%.cpp
	$(CPP) -c -o $@ $< $(CFLAGS)

-include $(DEP)

.PHONY: clean
clean:
	rm -f msxmulator tests
	rm -r -f $(DIRS_TO_MAKE)
	$(MAKE) -C $(TOOLS_DIR) clean
	$(MAKE) -C $(CMORE_DIR) clean

# Make the obj directory
$(shell mkdir -p $(DIRS_TO_MAKE))
