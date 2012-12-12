SML=../../SML
LIB=../../LIB
FSI=${SML}/fsi/*.fsi \
    ${LIB}/fsi/*.fsi
#    fsi/*.fsi
FS=${SML}/fs/*.fs \
   ${LIB}/fs/*.fs \
   fs/*.fs
TARGET=BarnesHut.exe

${TARGET} : ${FSI} ${FS}
	fsharpc $+ -o $@

clean :
	rm -f ${TARGET}

