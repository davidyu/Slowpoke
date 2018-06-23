@echo off
@if not exist build mkdir build
pushd build
cl @..\vcbuildflags
popd
