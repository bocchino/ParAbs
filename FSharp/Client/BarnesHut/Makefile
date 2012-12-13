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

${TARGET} : ${FSI} ${FS}
	fsharpc $+ -o $@

clean :
	rm -f ${TARGET}

