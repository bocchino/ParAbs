TARGET=barnes-hut
SIG=${wildcard sig/*.sig}
SML=${wildcard sml/*.sml}
ML_RUN=${PAR_ABS}/bin/ml-run
SIZE ?= 100000

all : ${TARGET}.x86-darwin

${TARGET}.x86-darwin : BarnesHut.cm ${SIG} ${SML}
	ml-build BarnesHut.cm BarnesHut.main ${TARGET}

test : ${TARGET}.x86-darwin
	bin/barnes-hut --size=${SIZE} --test

clean ::
	rm -f ${TARGET}.x86-darwin
