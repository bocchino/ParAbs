FSI=fsi/*.fsi
FS=fs/*.fs
TARGET=SML.dll

${TARGET} : ${FSI} ${FS}
	fsharpc -a $+ -o $@

clean :
	rm -f ${TARGET}

