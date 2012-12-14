SML=../../SML
LIB=../../LIB
FSI=${SML}/fsi/*.fsi \
    ${LIB}/fsi/*.fsi \
    fsi/*.fsi
FS=${SML}/fs/*.fs  \
   ${LIB}/fs/*.fs  \
   fs/Constants.fs \
   fs/Util.fs      \
   fs/Point.fs     \
   fs/Body.fs      \
   fs/DataGen.fs   \
   fs/Tree.fs      \
   fs/Gravity.fs   \
   fs/BarnesHut.fs
TARGET=BarnesHut.exe
SIZE ?= 64
SML_BH=../../../StandardML/Client/BarnesHut

${TARGET} : ${FSI} ${FS}
	${FSHARPC} $+  -o $@

run : ${TARGET}
	${MONO} $< --size=${SIZE} ${OPTS}

output ::
	${MONO} ${TARGET} --size=${SIZE} --output > output

test ::
	make output SIZE=${SIZE}
	make -C ${SML_BH} output SIZE=${SIZE}
	diff -u ./output ${SML_BH}/output

clean :
	rm -f ${TARGET} output

