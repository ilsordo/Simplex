# Simplex

#### Maxime LESOURD
#### Yassine HAMOUDI

https://github.com/nagaaym/Simplex

******************************************************************************

1. Dependencies
2. Compile and run
3. Produce a PDF file
4. Features

******************************************************************************


1. Dependencies
===============

The solver depends on ocaml >= 4.02.1, findlib, oasis, menhir and zarith.
It should be noted that zarith depends on libgmp.

You first need to install the following programs:
```
  sudo apt-get install m4
  sudo apt-get install libgmp-dev
  sudo apt-get install opam
```

Be sure to use the version 4.02.1 of opam, otherwise switch: 
``` 
  opam switch 4.02.1
  eval `opam config env`
```

Then you can install the packages using [opam](https://opam.ocaml.org/):
```
  opam install findlib oasis menhir zarith
```

2. Compile and run
==================

Compile the program:
```
  make
```

Run the simplex on the file test.lp:
```
  ./simplex test.lp
```

3. Produce a PDF file
=====================

You can produce a PDF file describing the execution of the simplex. To print it into record.pdf use the option:
```
  -print record.pdf
```

This file contains:
  - the different steps of the algorithm (dictionaries, entering/leaving variables)
  - the dual
  - the solution

4. Features
===========

The solver can use a range of backends using ```-field [num|gmp|float]```:
  - num : the standard OCaml arbitrary precision library
  - gmp : the zarith bindings to gmp
  - float : floating point numbers
