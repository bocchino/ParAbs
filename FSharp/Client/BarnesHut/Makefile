FSI=fsi/*.fsi
FS=fs/Constants.fs \
   fs/Util.fs      \
   fs/Point.fs     \
   fs/Body.fs      \
   fs/DataGen.fs   \
   fs/Tree.fs      \
   fs/Gravity.fs   \
   fs/BarnesHut.fs
TARGET=BarnesHut
SIZE ?= 64
DEPTH ?= 1
SML_BH=$(PAR_ABS)/StandardML/Client/BarnesHut
CLEAN=rm -f output

include ../Makefile.common

.PHONY : run-seq
run-seq : $(TARGET_SEQ)
	$(MONO) $< --size=$(SIZE) $(OPTS)

.PHONY : output-seq
output-seq ::
	$(MONO) $(TARGET_SEQ) --size=$(SIZE) --output > output

.PHONY : test-seq
test-seq :
	make output-seq SIZE=$(SIZE)
	make -C $(SML_BH) output SIZE=$(SIZE)
	diff -q ./output $(SML_BH)/output

.PHONY : run-par
run-par : $(TARGET_PAR)
	$(MONO) $< --size=$(SIZE) --depth=$(DEPTH) $(OPTS)

.PHONY : output-par
output-par ::
	$(MONO) $(TARGET_PAR) --size=$(SIZE) --depth=$(DEPTH) --output > output

.PHONY : test-par
test-par :
	make output-par SIZE=$(SIZE) DEPTH=$(DEPTH)
	make -C $(SML_BH) output SIZE=$(SIZE) DEPTH=$(DEPTH)
	diff -q ./output $(SML_BH)/output

.PHONY : test
test : test-seq test-par
