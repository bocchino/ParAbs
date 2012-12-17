TARGET=barnes-hut
SIG_LIB=../../Lib/sig/*.sig
SML_LIB=../../Lib/sml/*.sml
SIG=${wildcard sig/*.sig}
SML=${wildcard sml/*.sml}
ML_RUN=${PAR_ABS}/bin/ml-run
SIZE ?= 16000

all : ${TARGET}.x86-darwin

${TARGET}.x86-darwin : BarnesHut.cm ${SIG_LIB} ${SIG} ${SML_LIB} ${SML}
	ml-build BarnesHut.cm BarnesHut.main ${TARGET}

run : ${TARGET}.x86-darwin
	bin/barnes-hut --size=${SIZE}

output ::
	bin/barnes-hut --size=${SIZE} --test > $@

test : output
	make compare

compare :
	diff -q output ref-output/${SIZE}.txt

clean ::
	rm -f ${TARGET}.x86-darwin output
