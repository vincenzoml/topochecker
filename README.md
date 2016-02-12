Topochecker: a topological model checker
========================================

Topochecker is a spatio-temporal model checker based on closure spaces
and Kripke frames. Currently it checks a spatial extension of CTL
named STLCS (spatio-temporal logic of closure spaces).

Spatio-temporal model checking
==============================

Spatio-temporal model checking is perforemd with a variant of
Computation Tree Logic (CTL) featuring spatial operators coming from the
tradition of topological spatial logics.

CTL is widely known. See e.g.:

https://mitpress.mit.edu/books/principles-model-checking

The spatial operators come from the Spatial Logic of Closure Spaces
(SLCS), of which you can read in the following paper:

http://link.springer.com/chapter/10.1007%2F978-3-662-44602-7_18

http://arxiv.org/abs/1406.6393

https://raw.githubusercontent.com/vincenzoml/topochecker/master/doc/specifying_and_verifying_properties_of_space.pdf

The used algorithm is an extension of both CTL and SLCS model checking, described in a paper (to appear in VERY* 2015 @ SEFM):

https://raw.githubusercontent.com/vincenzoml/topochecker/master/doc/an_experimental_spatio-temporal_model_checker.pdf

For an introduction to topological spatial logics see

http://www.springer.com/us/book/9781402055867


Quick installation for the impatient running a debian-based distribution
========================================================================

Open a terminal and run the following four commands:

```
sudo apt-get install git ocaml-native-compilers libcsv-ocaml-dev libocamlgraph-ocaml-dev
git clone https://github.com/vincenzoml/topochecker.git 
cd topochecker
make
```

Optionally, type

```
make all
```

to also build the examples (some of them take plenty of space-time!)

Prerequisites
=============

The program is cross-platform. Prerequisites for compilation are:

- ocaml (best to use optimizing compiler ocamlopt.opt)
- the ocamlgraph library
- the ocamlcsv library


Installing prerequisites on ubuntu
----------------------------------

install the following packages using apt:

`ocaml ocaml-native-compilers libocamlgraph-ocaml-dev libcsv-ocaml-dev`


Installing prerequisites on OSX
-------------------------------

there may be more than one way, however the quickest path is to
install OPAM (e.g., from macports, ocamlbrew of from source):

https://opam.ocaml.org/doc/Install.html

and then install all the required dependencies using it:

```
opam init
opam install ocamlfind
opam install csv
opam install ocamlgraph
```

Compiling topochecker
=====================

Simply run

`make`

You can also use

`make all`

will also run the model checker on the examples in
the examples subdirectory.

BIG NOTE: topochecker uses ocamlbuild to build the tool, which
automatically handles dependencies; if ocamlbuild is not available,
the tool falls back to old-style makefile dependencies. If you have
troubles with ocamlbuild and want to use makefile dependencies, run
"make slow" in the tool directory; you can make that the default
target in the makefile; see the comment in src/Makefile for that.


Usage
=====

Usage:

`topochecker EXPERIMENT_FILENAME`

This will run the model checker on an experiment described by the file
at EXPERIMENT_FILENAME


Server mode
===========

Below, a long explanation of the syntax of experiments is
given. However, if no output file is specified, and no commands are
given, the system will run in "servere mode". In this mode,
topochecker will wait for input on stdin. The only command accepted is:

`Ask "ID" QFORMULA;
`Ask "ID" { "ID1", ... , "IDn" } QFORMULA;

ID is a string; QFORMULA is a quantitative formula, with syntax described later in this document. Result is a floating point value <VALUE> which is printed on stdout in the format "ID: <VALUE>". The optional list of identifiers in curly braces specifies a set of ids of space points; if such list is specified, then all counting formulas are restricted to the specified points (for example, if only one point is specified, then the result of a counting formula is either 0.0 or 1.0).

Syntax of experiment description files
======================================

EXPERIMENT_FILENAME, usually with ".topochecker" extension, consists
of a model declaration, an optional macro declaration part, and a
"commands" part (see files with extension .topochecker in the
examples).

Comments
--------

Comments can be introduced by "//"; everything from "//" to the end of
a line is ignored by the model checker.


Model declaration
-----------------


Dot file format:
----------------

Model declaration takes the form:

`Kripke "kripke.dot" Space "space.dot" Eval "eval.csv";`

Where "kripke.dot" "space.dot" and "eval.csv" are file names.

"kripke.dot" is a dot file containing a directed unlabelled graph,
representing a Kripke frame. Node ids must be numbers starting at 0
and with no gaps. The Kripke frame is completed by adding self-loops
to all nodes whose number of outgoing edges is 0.

"space.dot" is a dot file containing a directed unlabelled graph,
representing space.

Both graphs can be weighted; to achieve this, just add a property
"weight=n" to each edge, where n is a float; the default value for n
is 1.0.

"eval.csv" is a csv file, with three or more columns. Each row takes
the form

`state point prop1 prop2 ... propN`

where state is a node id in kripke.dot, space is a node id in
space.dot, and prop1 ... propN are strings (at least one must be
present) that are used as atomic propositions associated to state
"state" and point "point", giving rise to a spatio-temporal model;
alternatively, each of prop1 ... propN may be in the form string=int,
associating a quantity to a proposition. Actually, the first form is a
shorthand for string=1, and omitting a string makes its value equal to
0. Note that each pair of state and point id can be repeated many
times, if needed; different atomic propositions will be accepted in
different rows. However, for numeric values of atomic propositions, no
combination is done. The first value found in the csv is the one
accepted for the given atomic proposition if more than one row for the
same pair of ids, and the same atomic formula, is found.

Nifti medical imaging file format
---------------------------------

Model declaration takes the form

Space "space.nifti";

This requires medcon (open source converter for medical images) to be
available in the current path. The only atomic proposition is "value"
and it takes numeric values used in the same way as for dot
files. Output is written in raw+header file format.


Macro declaration
-----------------

Macro declaration takes the form of a list of statements of the form

`Let ide = FORMULA;`

or

`Let ide (arg1,...,argN) = FORMULA;`


Syntax of commands
------------------

Possible commands are:

`Check "COLOR" FMLA;`

g(mind the semicolon!). COLOR is an integer, which can also be in hexadecimal form (0xNNNNNN); this RGB color is used to color the output for the specified formula. Colours are currently just summed for different formulas.

`Output "filename";`
`Output "filename" state1,state2,...

Outputs to a set of files named "filename-STATEID", closing the
previous ones.  The output is written as OUTPUT_PREFIX-nn.dot where nn
is replaced by the name of each state in the Kripke structure of the
experiment, and each file OUTPUT_PREFIX-nn.dot is a graphical
representation of the evaluation of formulas in the experiment over
space, in the dot file format. If a list of states is additionally
specified, then only these states are saved.

Syntax of formulas
------------------

Formulas are described by the following syntax:

```
FMLA ::=
         [string]                       (atomic proposition, no quotes around the string; see below for reserved names)
       | [string OP INTEGER]		(quantitative check, OP is <, <=, ==, !=, >, >=)
       | TT                             (true)
       | FF                             (false)
       | FMLA & FMLA			(and)
       | FMLA | FMLA 			(or)
       | (FMLA)                         (subformula)
       | identifier                     (as declared with Let ide = FMLA)
       | identifier(arg1,...,argN)      (as declared with Let ide(arg1,...,argN) = FMLA)
       | N FMLA                         (near in space: reachable in one step)
       | N^k FMLA                       (k is a number; nested application of N)
       | I FMLA                         (dual of N: not reaching in one step)
       | FMLA S FMLA                    (surrounded in space)
       | E X FMLA                       (EX from CTL)
       | A X FMLA                       (AX from CTL)
       | E G FMLA                       (EG from CTL)            
       | A G FMLA                       (AG from CTL)
       | E F FMLA                       (EF from CTL)
       | A F FMLA                       (AF from CTL)
       | E FMLA U FMLA                  (EU from CTL)
       | A FMLA U FMLA                  (AU from CTL)
```

Quantitative formulas are described by the following syntax

QFMLA ::=
         float				(floating point constant)
       | (QFMLA)			(subformula)
       | QFMLA OP QFMLA 		(quantitative check, OP as above)
       | # QFMLA  			(count points in space satisfying QFMLA in state 0)	

Special treatment of deadlocks and reserved names of atomic propositions
========================================================================

It may happen that there are deadlock states in a Kripke structure,
that is, states with no outgoing edges. In that case, usually the
structure is completed with self-loops to make sure that only infinite
paths are considered. Additionally, we make such states observable, by
adding an atomic predicate "deadlock" that is true only on these
states; this means that the name "deadlock" should be avoided for
atomic propositions.

See also
========

Previous iteration of the tool, loading spatio-temporal models based
on images:

https://github.com/cherosene/ctl_logic
