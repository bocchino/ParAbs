SML=../SML
FSI=${SML}/fsi/*.fsi fsi/*.fsi
FS=${SML}/fs/*.fs fs/*.fs
FS_SEQ=${SML}/fs/*.fs fs/seq/*.fs
FS_PAR=${SML}/fs/*.fs fs/par/*.fs
TARGET_SEQ=ParAbs-seq.dll
TARGET_PAR=ParAbs-par.dll

all : $(TARGET_SEQ) $(TARGET_PAR)

${TARGET_SEQ} : ${FSI} ${FS}
	make -C fs
	fsharpc -a ${FSI} ${FS_SEQ} -o $@

${TARGET_PAR} : ${FSI} ${FS}
	make -C fs
	fsharpc -a ${FSI} ${FS_PAR} -o $@

clean :
	rm -f ${TARGET_SEQ} ${TARGET_PAR}
	make -C fs clean

