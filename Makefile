.PHONY: Slowpoke clean

SRC=src
DIST=dist

Slowpoke:
	ghc -i${SRC}/ --make Main -o ${DIST}/Slowpoke -O2

clean:
	pushd ${SRC} && rm *.hi *.o *.dyn_hi *.dyn_o && popd
