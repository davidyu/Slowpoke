.PHONY: Slowpoke

SRC=src
DIST=dist

Slowpoke:
	ghc -i${SRC}/; -prof -auto-all -caf-all -O2 --make Main -o ${DIST}/Slowpoke
