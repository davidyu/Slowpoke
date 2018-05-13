.PHONY: Slowpoke clean

SRC=src
DIST=dist

Slowpoke:
	mkdir -p ${DIST}
	mkdir -p ${DIST}/obj
	ghc -i${SRC}/ --make Main -outputdir ${DIST}/obj -o ${DIST}/Slowpoke -O2

clean:
	pushd ${SRC} && rm *.hi *.o *.dyn_hi *.dyn_o && popd
