TARGET=barnes-hut
SIG=${wildcard sig/*.sig}
SML=${wildcard sml/*.sml}
ML_RUN=${PAR_ABS}/bin/ml-run
SIZE ?= 100000

all : ${TARGET}.x86-darwin

${TARGET}.x86-darwin : BarnesHut.cm ${SIG} ${SML}
	ml-build BarnesHut.cm BarnesHut.main ${TARGET}

run : ${TARGET}.x86-darwin
	bin/barnes-hut --size=${SIZE}

output ::
	bin/barnes-hut --size=${SIZE} --test > $@

test : output
	diff output ${DPJ_ROOT}/Benchmarks/Applications/BarnesHut/output

clean ::
	rm -f ${TARGET}.x86-darwin output
