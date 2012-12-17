FS=ArraySlice.fs DisjointArraySlices.fs MergeSort.fs
TARGET=MergeSort
TARGET_SEQ=$(TARGET)-seq.exe
TARGET_PAR=$(TARGET)-par.exe

.PHONY : run-seq
run-seq : $(TARGET_SEQ)
	$(MONO) $< $(OPTS)

.PHONY : run-par
run-par : $(TARGET_PAR)
	$(MONO) $< $(OPTS)

$(TARGET_SEQ) : $(FS)
ifndef FSHARPC
	$(error Set variable FSHARPC to your F# compiler first)
endif
	$(FSHARPC) $(FS) -o $@

$(TARGET_PAR) : $(FS)
ifndef FSHARPC
	$(error Set variable FSHARPC to your F# compiler first)
endif
	$(FSHARPC) -d PARALLEL $(FS) -o $@

