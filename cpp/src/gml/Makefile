.PHONY: test folders update

BIN=bin
SHELL=/bin/bash
CFLAGS=-std=c++11 -I./src
TESTFLAGS=$(CFLAGS) -I./test
SRCCPP=$(shell find ./src -name "*.cpp")
TESTCPP=$(shell find ./test -name "*.cpp")
LIB=./test/lib

test: folders update
	$(CXX) $(TESTFLAGS) -o $(BIN)/$@ $(SRCCPP) $(TESTCPP)
	@pushd $(BIN) > /dev/null && ./test && popd > /dev/null

folders:
	@mkdir -p $(BIN)

update:
	@pushd $(LIB) > /dev/null && sh update.sh && popd > /dev/null
