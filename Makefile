.PHONY: Slowpoke

SRC=src
DIST=dist

Slowpoke:
	ghc -i${SRC}/ --make Main -o ${DIST}/Slowpoke -O2
