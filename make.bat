@set SRC=src
@set DIST=dist

@if not exist %DIST% mkdir %DIST%
@if not exist %DIST%\obj mkdir %DIST%\obj

ghc -i%SRC%\ --make Main -outputdir %DIST%\obj -o %DIST%\Slowpoke -O2
