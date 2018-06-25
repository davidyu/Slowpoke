#!/bin/bash

# usage: download url target
# where target is the name of the local file target
download() {
    local url="$1"
    local target="$2"
    [ -f "$target" ] || curl -k "$url" -o "$target"
}

echo "updating vendor code..."
download https://raw.githubusercontent.com/catchorg/Catch2/master/single_include/catch2/catch.hpp catch.hpp
echo "done."
