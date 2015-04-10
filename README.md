Topochecker: a topological model checker
========================================


INTRODUCTION

This project is a research prototype, implementing a model checker
that operates on "space" in the sense of a model described by
"topological" means, or, more precisely, by the means of Closure
Spaces. Closure spaces encompass topological spaces and graph-like
structures, weakening the idempotency requirement of topological
closure.

The core implementation is independent of the chosen closure space,
however, currently, the logical language is tailored to a specific
implementation, 2d coloured bitmaps.


PREREQUISITES

The prerequisites are the ocaml compiler, lexer and parser generators
(ocamllex and ocamlyacc), and the camlimages library. On ubuntu,
install the packages:

    ocaml-native-compilers
    ocaml-nox
    libcamlimages-ocaml-dev

Optionally, you can install either the package

    rlfe

or

    rlwrap

to get command-line editing, and permanent history in the .hist file
in the current directory.


COMPILING

To compile, type "make" in the "src" subdirectory.


RUNNING

Running "make interactive-maze" will invoke a session that loads the
maze examle.

The tool is named "ccsmc" and can be invoked with 1 to 3
arguments. The first argument is mandatory and is the name of a
24-bits bmp file (more formats can be supported easily if needed). The
second argument, if provided, is the name of a text file containing
definitions to be parsed before running the model checker. The third
argument, if provided, switches on batch execution (instead of an
interactive prompt). In this mode, argument 3 is the base name of a
series of bitmap files that will be produced, one by one, by the
"Paint" commands encountered in the file provided as second
argument. Unfortunately the results of "Ask" commands are not saved
anywhere. In future releases, we will implement a conditional Paint
command that will paint a region depending on a global formula, thus
providing means to represent the results of Ask.

The tool provides let binding for global formulas (see the provided
examples) and two commands: "Paint" and "Ask". "Paint" takes as input
a colour and a formula, and colours the pixels in the image satisfying
the formula. "Ask" takes as input a global formula and returns True
whether the formula is satisfied, False otherwise. See the examples
for more details.



MORE INFORMATION

This documentation will be updated as soon as a more stable version of
the tool and the input language of the model checker will be
available. 


For more information, please write to

Vincenzo Ciancia - vincenzoml@gmail.com

