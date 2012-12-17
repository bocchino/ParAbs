Scripts
=======

Some utility scripts for use with the ParAbs library.

translateargs.py
----------------

This program is a general-purpose tool for translating command-line
arguments, e.g. from Unix to Windows form and vice versa.  

**Example:** If your makefile says $(FSHARPC) A1 ... An, where setting
FSHARPC=fsharpc works on Unix, then on windows (inside Cygwin) you
could set FSHARPC to

`python /path/to/translateargs.py fsc cygpath::-w`

and that should be the same as if you had said

`fsc A1w ... Anw`

directly in the makefile, where `cygpath -w Ai` generates `Aiw` for
all i.

**More generally:** Say you write a script that invokes command C with
arguments A1 ... An, and some platforms require C' A1' ... An'.  Say
you have a tool T that will convert any Ai to Ai'.  Then using this
tool you can say

`python translateargs.py C' T A1 ... An`

and the effect should be the same as if you had said

`C' A1' ... An'`

The only catch is that because of command-line parsing issues, you
don't write spaces when invoking either C' or T; instead you write

`::`




