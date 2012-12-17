Scripts
=======

Some utility scripts for use with the ParAbs library.

translateargs.py
----------------

This program is a general-purpose tool for translating command-line
arguments, e.g. from Unix to Windows form and vice versa.  Say you
write a makefile that invokes command C with arguments A1 ... An, and
the windows version of C (call it Cw) wants arguments A1w ... Anw.
Suppose you also have a utility T that will translate one argument for
you.  Then using this tool you can say

`python translateargs.py Cw T A1 ... An`

and the effect should be the same as if you had said

`Cw A1w ... Anw`

The only catch is that because of command-line parsing issues, you
don't write spaces when invoking either Cw or T; instead you write

`::`

*Example:* If your makefile says $(FSHARPC) A1 ... An, where setting
FSHARPC=fsharpc works on Unix, then on windows (inside Cygwin) you
could set FSHARPC to

`python /path/to/translateargs.py fsc cygpath::-w`

and that should be the same as if you had said

`fsc A1w ... Anw`

directly in the makefile, where `cygpath -w Ai` generates `Aiw` for
all i.



