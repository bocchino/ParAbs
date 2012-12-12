SML=../SML
FSI=${SML}/fsi/*.fsi fsi/*.fsi
FS=${SML}/fs/*.fs fs/*.fs
TARGET=ParAbs.dll

${TARGET} : ${FSI} ${FS}
	fsharpc -a $+ -o $@

clean :
	rm -f ${TARGET}

