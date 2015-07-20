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
make all
```

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
opam init --comp 4.02.1
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

`topochecker EXPERIMENT_FILENAME OUTPUT_PREFIX`

This will run the model checker on an experiment described by the file
at EXPERIMENT_FILENAME and write the output as OUTPUT_PREFIX-nn.dot
where nn is replaced by the name of each state in the Kripke structure
of the experiment, and each file OUTPUT_PREFIX-nn.dot is a graphical
representation of the evaluation of formulas in the experiment over
space, in the dot file format.


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
0.


Macro declaration
-----------------

Macro declaration takes the form of a list of statements of the form

`Let ide = FORMULA;`

or

`Let ide (arg1,...,argN) = FORMULA;`


Syntax of commands
------------------

Possible commands are:

`Check "COLOR" FORMULA;`

(mind the semicolon!). COLOR is an integer, which can also be in hexadecimal form (0xNNNNNN); this RGB color is used to color the output for the specified formula. Colours are currently just summed for different formulas.

`Output "filename";`

Starts a new file named "filename", closing the previous one. If no
"Output" commands are present, the second argument from the command
line is used.

Syntax of formulas
------------------

Formulas are described by the following syntax:

```
FMLA ::=
         [string]                       (atomic proposition, no quotes around the string)
       | [string OP INTEGER]		(quantitative check, OP is <, <=, ==, !=, >, >=)
       | TT                             (true)
       | FF                             (false)
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

See also
========

Previous iteration of the tool, loading spatio-temporal models based
on images:

https://github.com/cherosene/ctl_logic
