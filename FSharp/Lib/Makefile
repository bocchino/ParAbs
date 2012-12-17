SML=../SML
FSI=${SML}/fsi/*.fsi fsi/*.fsi
FS=${SML}/fs/*.fs fs/*.fs
TARGET_SEQ=ParAbs-seq.dll
TARGET_PAR=ParAbs-par.dll

all : $(TARGET_SEQ) $(TARGET_PAR)

${TARGET_SEQ} : ${FSI} ${FS}
	fsharpc -a $+ -o $@

${TARGET_PAR} : ${FSI} ${FS}
	fsharpc -a -d:PARALLEL $+ -o $@

clean :
	rm -f ${TARGET_SEQ} ${TARGET_PAR}

