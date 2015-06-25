Topochecker: a topological model checker
========================================

Topochecker is a spatio-temporal model checker based on closure spaces
and Kripke frames. Currently it checks a spatial extension of CTL
named STLCS (spatio-temporal logic of closure spaces).



Prerequisites
=============

The program is cross-platform. Prerequisites for compilation are:

- ocaml (best to use optimizing compiler ocamlopt.opt)
- the ocamlgraph library
- the ocamlcsv library


Installing prerequisites on ubuntu
----------------------------------

install the following packages using apt:

ocaml ocaml-native-compilers libocamlgraph-ocaml-dev libcsv-ocaml-dev


Installing prerequisites on OSX
-------------------------------

there may be more than one way, however the quickest path is to
install OPAM (e.g., from macports, ocamlbrew of from source):

https://opam.ocaml.org/doc/Install.html

and then install all the required dependencies using it:

`
opam init --comp 4.02.1
opam install ocamlfind
opam install csv
opam install ocamlgraph
`

Compiling topochecker
=====================

Run make.

Using "make all" will also run the model checker on the examples in
the examples subdirectory.


Usage
=====

Usage:

topochecker EXPERIMENT_FILENAME OUTPUT_PREFIX

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


Model declaration
-----------------

Model declaration takes the form:

Kripke "kripke.dot" Space "space.dot" Eval "eval.csv";

Where "kripke.dot" "space.dot" and "eval.csv" are file names.

"kripke.dot" is a dot file containing a directed unlabelled graph,
representing a Kripke frame. Node ids must be numbers starting at 0
and with no gaps.

"space.dot" is a dot file containing a directed unlabelled graph,
representing space. Node ids must be numbers starting at 0 and with no
gaps.

"eval.csv" is a whitespace-separated csv file, with three or more
columns. Each row takes the form

state point prop1 prop2 ... propN

where state is a node id in kripke.dot, space is a node id in
space.dot, and prop1 ... propN are strings (at least one must be
present) that are used as atomic propositions associated to state
"state" and point "point", giving rise to a spatio-temporal model.


Macro declaration
-----------------

Macro declaration takes the form of a list of statements of the form

Let ide = FORMULA;

or

Let ide (arg1,...,argN) = FORMULA;


Syntax of commands
------------------

The only supported command at the moment is

Check FORMULA;

(mind the semicolon!).

Formulas are described by the following syntax:

`
FMLA ::=
	 [string]                       (atomic proposition, no quotes around the string)
       | T                              (true)
       | F                              (false)
       | (FMLA)				(subformula)
       | identifier                     (as declared with Let ide = FMLA)
       | identifier(arg1,...,argN)	(as declared with Let ide(arg1,...,argN) = FMLA)
       | N FMLA				(near in space: reachable in one step)
       | N^k FMLA			(k is a number; nested application of N)
       | I FMLA				(dual of N: not reaching in one step)
       | FMLA S FMLA			(surrounded in space)
       | E X FMLA			(EX from CTL)
       | A X FMLA			(AX from CTL)
       | E G FMLA			(EG from CTL)       	 
       | A G FMLA			(AG from CTL)
       | E F FMLA			(EF from CTL)
       | A F FMLA			(AF from CTL)
       | E FMLA U FMLA			(EU from CTL)
       | A FMLA U FMLA			(AU from CTL)
`

See also
========

Previous iteration of the tool, loading spatio-temporal models based
on images:

https://github.com/cherosene/ctl_logic
