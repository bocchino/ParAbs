ParAbs
======

This is the code base for ParAbs, a library of high-level abstractions
that support safe parallel programming with imperative updates.

License
-------

This software is free for general use, provided that attribution is
maintained (citing this repository is sufficient).

This software is provided with absolutely no warranty of any kind.  It
is a research prototype and is undergoing active development.

Overview
--------

The subdirectory `StandardML` provides the reference version of the
library, together with some sample client code.  This version is
written in Standard ML (SML).  Because SML is a sequential language,
this implementation of the library is parallel.

The subdirectory `FSharp` provides an F# port of the library.  It is a
parallel implementation.

Dependencies
------------

To use the SML version of the library, you must install Standard ML of
New Jersey (SML/NJ):

http://www.smlnj.org/dist/working/110.74/index.html

To use the F# version of the library, you must have a working F#
installation.  On a non-Windows machine, you can get F# by first
installing Mono:

http://www.mono-project.com/Main_Page

Setup
-----

1. To use either version of the library, you must set the environment
   variable `PAR_ABS` to point to the top of the installation tree (i.e.,
   the directory in which this file is locate).

2. To use the SML/NJ version of the library, you must set the environment
   variable `SML_NJ` to point your SML/NJ installation.

3. To use the F# version of the library, you must set the environment
   variable `FSHARPC` equal to the command used to invoke the F#
   compiler (typically `fsc` in Windows, `fsharpc` for Mono).
   Additionally, if you are using Mono, then you must set the variable
   `MONO` equal to the command used to invoke Mono (typically `mono`).
