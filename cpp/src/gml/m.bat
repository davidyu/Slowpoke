@setlocal EnableDelayedExpansion

@set SRC=src
@set DIST=bin
@set TEST=test
@set BUILD_USING_VISUAL_STUDIO=1

@echo updating vendor code...

@pushd %TEST%\lib
call update
@popd

@echo building test code...

@if not exist %DIST% mkdir %DIST%

@if %BUILD_USING_VISUAL_STUDIO%==1 (
    @pushd %DIST%
    cl @..\vcbuildflags
    @popd
) else (
    g++ -std=c++11 -I./src -I./test -o %DIST%/test.exe ./test/test.cpp ./test/mat.cpp ./test/vec.cpp
)
