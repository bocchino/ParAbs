FS=./*.fs

all : ${FS} par seq
	python libgen.py

par :
	mkdir $@

seq :
	mkdir $@

clean :
	rm -Rf par seq

.PHONY :  all clean