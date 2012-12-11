TARGET=mergesort
SIG=${wildcard sig/*.sig}
SML=${wildcard sml/*.sml}
SIZE ?= 1024

all : ${TARGET}.x86-darwin

${TARGET}.x86-darwin : MergeSort.cm ${SIG} ${SML}
	ml-build MergeSort.cm MergeSort.main ${TARGET}

test : ${TARGET}.x86-darwin
	bin/mergesort ${SIZE}

clean ::
	rm -f ${TARGET}.x86-darwin
